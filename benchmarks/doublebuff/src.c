#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<string.h>
#include<pthread.h>
#include <sys/time.h>

static inline double get_time()
{
    struct timeval t;
    gettimeofday(&t, NULL);
    return t.tv_sec + t.tv_usec*1e-6;
}

#define REPETITIONS 50

#define B_SIZE 4096
#define N_REP 1000

int rep = N_REP;

typedef struct wait_mutex {
        volatile int ready;
        pthread_mutex_t mut;
        pthread_cond_t cond;
    } wait_mutex_t;

void send(wait_mutex_t * w){
    pthread_mutex_lock(&w -> mut);
    if (!w -> ready){
	    pthread_cond_wait(&w -> cond, &w -> mut);
    }
    w -> ready = 0;
    pthread_mutex_unlock(&w -> mut);
}
void recv(wait_mutex_t * w){
    pthread_mutex_lock(&w -> mut);
    w -> ready = 1;
    pthread_mutex_unlock(&w -> mut);
    pthread_cond_signal(&w -> cond);
}

void write_buff(int *b) {
    for(int i = 0; i < B_SIZE; i++) {
        b[i] = i + 10;
    }
}

void compute_buff(int *b) {
    for(int i = 0; i < B_SIZE; i++) {
        b[i] = log(b[i]);
    }
}

void read_buff(int *b) {
    int buf[B_SIZE];
    memcpy(buf, b, sizeof(int) * B_SIZE);
    // for(int i = 0; i < B_SIZE; i++) {
    // //    printf("%d ", b[i]);
    // }
    //printf("\n");
}

wait_mutex_t srv_rdy_A = {0, PTHREAD_MUTEX_INITIALIZER, PTHREAD_COND_INITIALIZER};
wait_mutex_t srv_rdy_B = {0, PTHREAD_MUTEX_INITIALIZER, PTHREAD_COND_INITIALIZER};

wait_mutex_t sink_rdy_A = {0, PTHREAD_MUTEX_INITIALIZER, PTHREAD_COND_INITIALIZER};
wait_mutex_t sink_rdy_B = {0, PTHREAD_MUTEX_INITIALIZER, PTHREAD_COND_INITIALIZER};

wait_mutex_t src_cpy_A = {0, PTHREAD_MUTEX_INITIALIZER, PTHREAD_COND_INITIALIZER};
wait_mutex_t src_cpy_B;

wait_mutex_t srv_cpy_A = {0, PTHREAD_MUTEX_INITIALIZER, PTHREAD_COND_INITIALIZER};
wait_mutex_t srv_cpy_B = {0, PTHREAD_MUTEX_INITIALIZER, PTHREAD_COND_INITIALIZER};

int buffA[B_SIZE]; 
int buffB[B_SIZE]; 

void *source(void *arg){
    int n_rep = rep;
    while(n_rep--){
        write_buff((int *)buffA);
    }
    return NULL;
}

void *service(void *arg){
    int n_rep = rep;
    while(n_rep--){
        compute_buff((int *)buffA);
        compute_buff((int *)buffB);
    }
    return NULL;
}

int main(int argc, char * argv[]){

    double time = 0;
    double time_diff = 0;
    double time_old = 0;
    double var = 0;
    double start = 0;
    double end = 0;

    for(int i=0; i<REPETITIONS; i++){
        start = get_time();
        source(NULL);
        end = get_time();
        time_diff = end - start;
        time_old = time;
        time += (time_diff - time)/(i+1);
        var += (time_diff - time) * (time_diff - time_old); 
    }

    printf("mean: %f\n", time);
    printf("stddev: %f\n", REPETITIONS<=1? 0: sqrt(var / (REPETITIONS - 1)));
}