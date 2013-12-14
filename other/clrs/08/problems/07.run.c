#include "07.test.c"

#include <sys/time.h>
#ifdef __MACH__
#include <mach/clock.h>
#include <mach/mach.h>
#endif

void current_utc_time(struct timespec *ts) {
#ifdef __MACH__
  clock_serv_t cclock;
  mach_timespec_t mts;
  host_get_clock_service(mach_host_self(), CALENDAR_CLOCK, &cclock);
  clock_get_time(cclock, &mts);
  mach_port_deallocate(mach_task_self(), cclock);
  ts->tv_sec = mts.tv_sec;
  ts->tv_nsec = mts.tv_nsec;
#else
  clock_gettime(CLOCK_REALTIME, ts);
#endif
}

double elapsed;
struct timespec start, finish;

#define TIME(message, sorting) \
    randomize(array, NUMBERS); \
    current_utc_time(&start); \
    sorting; \
    current_utc_time(&finish); \
    elapsed = (finish.tv_sec - start.tv_sec); \
    elapsed += (finish.tv_nsec - start.tv_nsec) / 1000000000.0;\
    printf(message " = %f\n", (double) elapsed); \
    assert_sorted(array, NUMBERS);

const size_t COLUMNS = 180;
const size_t ROWS = 2 * COLUMNS * COLUMNS;
const size_t NUMBERS = ROWS * COLUMNS;

int main() {
    number *array = calloc(NUMBERS, sizeof(number));

    TIME("stdlib sort", { sort(array, (column_t) {0, NUMBERS}); });
    TIME("columnsort ", { columnsort(array, ROWS, COLUMNS, threaded_sort_columns); });

    return 0;
}
