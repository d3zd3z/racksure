/* Use the stat call, but in a portable output format. */

#define _FILE_OFFSET_BITS 64

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <dirent.h>
#include <stdint.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

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
