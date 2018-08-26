// taken from: https://github.com/majek/dump/blob/master/msqueue/pthread_spin_lock_shim.h
#include <errno.h>

#ifdef __APPLE__

typedef int pthread_spinlock_t;

static inline int pthread_spin_init(pthread_spinlock_t *lock, int pshared) {
	__asm__ __volatile__ ("" ::: "memory");
	*lock = 0;
	return 0;
}

static inline int pthread_spin_destroy(pthread_spinlock_t *lock) {
	return 0;
}

static inline int pthread_spin_lock(pthread_spinlock_t *lock) {
	while (1) {
		int i;
		for (i=0; i < 10000; i++) {
			if (__sync_bool_compare_and_swap(lock, 0, 1)) {
				return 0;
			}
		}
		sched_yield();
	}
}

static inline int pthread_spin_trylock(pthread_spinlock_t *lock) {
	if (__sync_bool_compare_and_swap(lock, 0, 1)) {
		return 0;
	}
	return EBUSY;
}

static inline int pthread_spin_unlock(pthread_spinlock_t *lock) {
	__asm__ __volatile__ ("" ::: "memory");
	*lock = 0;
	return 0;
}

#endif
