/* Use the stat call, but in a portable output format. */

/* Make sure we use 64 bit offsets on 32-bit platforms. */
#define _FILE_OFFSET_BITS 64

/* Needed to get O_NOATIME on Linux */
#define _GNU_SOURCE

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <dirent.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include <openssl/sha.h>

#define HASH_BUF_SIZE 65535

struct portstat {
	int64_t dev;
	int64_t ino;
	int64_t nlink;
	int64_t mode;
	int64_t uid;
	int64_t gid;
	int64_t rdev;
	int64_t size;
	int64_t blksize;
	int64_t blocks;
	uint64_t atime_sec;
	uint64_t atime_nsec;
	uint64_t mtime_sec;
	uint64_t mtime_nsec;
	uint64_t ctime_sec;
	uint64_t ctime_nsec;
};

#if defined(__linux__)
#define ATIME_FIELD st_atim
#define MTIME_FIELD st_mtim
#define CTIME_FIELD st_ctim
#elif defined(__APPLE__) && defined(__MACH__)
#define ATIME_FIELD st_atimespec
#define MTIME_FIELD st_mtimespec
#define CTIME_FIELD st_ctimespec
#else
#  error "Unsupported platform"
#endif

int portable_lstat(const char *pathname, struct portstat *pbuf)
{
	struct stat buf;

	int res = lstat(pathname, &buf);

	if (res == 0) {
		pbuf->dev = buf.st_dev;
		pbuf->ino = buf.st_ino;
		pbuf->nlink = buf.st_nlink;
		pbuf->mode = buf.st_mode;
		pbuf->uid = buf.st_uid;
		pbuf->gid = buf.st_gid;
		pbuf->rdev = buf.st_rdev;
		pbuf->size = buf.st_size;
		pbuf->blksize = buf.st_blksize;
		pbuf->blocks = buf.st_blocks;

		pbuf->atime_sec = buf.ATIME_FIELD.tv_sec;
		pbuf->atime_nsec = buf.ATIME_FIELD.tv_nsec;
		pbuf->mtime_sec = buf.MTIME_FIELD.tv_sec;
		pbuf->mtime_nsec = buf.MTIME_FIELD.tv_nsec;
		pbuf->ctime_sec = buf.CTIME_FIELD.tv_sec;
		pbuf->ctime_nsec = buf.CTIME_FIELD.tv_nsec;
	}

	return res;
}

/* Types are more simple here. */
#define KIND_DIR  'd'
#define KIND_OTHER '.'
#define KIND_UNKNOWN 'u'

struct portdirent {
	int64_t ino;
	char kind;
	char name[256];
};

DIR *portable_opendir(const char *name)
{
	return opendir(name);
}

int portable_closedir(DIR *dirp)
{
	return closedir(dirp);
}

int portable_readdir(DIR *dirp, struct portdirent *pent)
{
	errno = 0;
	struct dirent *ent = readdir(dirp);

	if (ent != NULL) {
		pent->ino = ent->d_ino;
		switch (ent->d_type) {
		case DT_UNKNOWN:
			pent->kind = KIND_UNKNOWN;
			break;
		case DT_DIR:
			pent->kind = KIND_DIR;
			break;
		default:
			pent->kind = KIND_OTHER;
			break;
		}
		if (strlen(ent->d_name) > 255) {
			fprintf(stderr, "Name too long returned\n");
			errno = ENOMEM;
			return -1;
		}

		strcpy(pent->name, ent->d_name);

		return 0;
	} else {
		return -1;
	}
}

/* Make sure we get an error if noatime isn't found on Linux. */
#if !defined(__linux__) && !defined(O_NOATIME)
#  define O_NOATIME 0
#endif

/*
 * Read the contents of the given file, computing its sha1 hash,
 * storing the result in 'out'.  Returns 0 on success, or -1 and sets
 * errno on an error.
 */
int sha1_file_contents(const char *name, unsigned char *out)
{
	/* Try to open with no-atime, and if that fails, use a regular
	 * open. */
	int fd = open(name, O_RDONLY | O_NOATIME);
	if (fd < 0) {
		fd = open(name, O_RDONLY);
	}
	if (fd < 0) {
		return fd;
	}

	/* TODO: Determine if lots of allocate and free here is a
	 * performance issue. */
	unsigned char *buf = malloc(HASH_BUF_SIZE);
	if (buf == NULL) {
		(void)close(fd);
		errno = ENOMEM;
		return -1;
	}

	SHA_CTX ctx;
	SHA1_Init(&ctx);

	while (1) {
		int res = read(fd, buf, HASH_BUF_SIZE);
		if (res == 0) {
			break;
		}
		if (res < 0) {
			free(buf);
			(void)close(fd);
			return res;
		}

		SHA1_Update(&ctx, buf, res);
	}

	free(buf);
	(void)close(fd);

	SHA1_Final(out, &ctx);
	return 0;
}
