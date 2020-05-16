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

#define B_SIZE 4096
#define N_REP 1000
#define REPETITIONS 50

int rep = N_REP;

typedef struct wait_mutex {
        volatile int ready;
        pthread_mutex_t mut;
        pthread_cond_t snd;
        pthread_cond_t rcv;
    } wait_mutex_t;

void send(wait_mutex_t * w){
    pthread_mutex_lock(&w -> mut);
    while (w -> ready) {
      pthread_cond_wait(&w -> snd, &w-> mut);
    }
    w -> ready = 1;
    pthread_mutex_unlock(&w -> mut);
    pthread_cond_signal(&w -> rcv);
}
void recv(wait_mutex_t * w){
    pthread_mutex_lock(&w -> mut);
    while (!w -> ready) {
      pthread_cond_wait(&w -> rcv, &w-> mut);
    }
    w -> ready = 0;
    pthread_mutex_unlock(&w -> mut);
    pthread_cond_signal(&w -> snd);
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
    //  printf("%d ", b[i]);
    // }
    // printf("\n");
}

wait_mutex_t srv_rdy_A = {0, PTHREAD_MUTEX_INITIALIZER, PTHREAD_COND_INITIALIZER, PTHREAD_COND_INITIALIZER};
wait_mutex_t srv_rdy_B = {0, PTHREAD_MUTEX_INITIALIZER, PTHREAD_COND_INITIALIZER, PTHREAD_COND_INITIALIZER};

wait_mutex_t sink_rdy_A = {0, PTHREAD_MUTEX_INITIALIZER, PTHREAD_COND_INITIALIZER, PTHREAD_COND_INITIALIZER};
wait_mutex_t sink_rdy_B = {0, PTHREAD_MUTEX_INITIALIZER, PTHREAD_COND_INITIALIZER, PTHREAD_COND_INITIALIZER};

wait_mutex_t src_cpy_A = {0, PTHREAD_MUTEX_INITIALIZER, PTHREAD_COND_INITIALIZER, PTHREAD_COND_INITIALIZER};
wait_mutex_t src_cpy_B = {0, PTHREAD_MUTEX_INITIALIZER, PTHREAD_COND_INITIALIZER, PTHREAD_COND_INITIALIZER};

wait_mutex_t srv_cpy_A = {0, PTHREAD_MUTEX_INITIALIZER, PTHREAD_COND_INITIALIZER, PTHREAD_COND_INITIALIZER};
wait_mutex_t srv_cpy_B = {0, PTHREAD_MUTEX_INITIALIZER, PTHREAD_COND_INITIALIZER, PTHREAD_COND_INITIALIZER};

int buffA[B_SIZE];
int buffB[B_SIZE];

void *source(void *arg){
    int n_rep = rep;
    while(n_rep--){
        recv(&srv_rdy_A);
        write_buff((int *)buffA);
        send(&src_cpy_A);

        recv(&srv_rdy_B);
        write_buff((int *)buffB);
        send(&src_cpy_B);
    }
    return NULL;
}

void *service(void *arg){
    int n_rep = rep;
    // send(&srv_rdy_A);
    // send(&srv_rdy_B);
    while(n_rep--){
        send(&srv_rdy_A);
        recv(&src_cpy_A);
        compute_buff((int *)buffA);
        recv(&sink_rdy_A);
        send(&srv_cpy_A);
        // send(&srv_rdy_A);

        send(&srv_rdy_B);
        recv(&src_cpy_B);
        compute_buff((int *)buffB);
        recv(&sink_rdy_B);
        send(&srv_cpy_B);
        // send(&srv_rdy_B);
    }
    return NULL;
}

void *sink(void *arg){
    int n_rep = rep;
    while(n_rep--){
        send(&sink_rdy_A);
        recv(&srv_cpy_A);
        read_buff((int *)buffA);

        send(&sink_rdy_B);
        recv(&srv_cpy_B);
        read_buff((int *)buffB);
    }
    return NULL;
}

void reset() {
  srv_rdy_A.ready = 0;
  srv_rdy_B.ready = 0;
  sink_rdy_A.ready = 0;
  sink_rdy_B.ready = 0;
  src_cpy_A.ready = 0;
  src_cpy_B.ready = 0;
  srv_cpy_A.ready = 0;
  srv_cpy_B.ready = 0;
}

int main(int argc, char * argv[]){
    pthread_t p_src, p_srv;

    double time = 0;
    double time_diff = 0;
    double time_old = 0;
    double var = 0;
    double start = 0;
    double end = 0;

    for(int i=0; i<REPETITIONS; i++){
        reset();
        start = get_time();
        pthread_create(&p_src, NULL, &source, NULL);
        pthread_create(&p_srv, NULL, &service, NULL);

        sink(NULL);
        pthread_join(p_src, NULL);
        pthread_join(p_srv, NULL);
        end = get_time();
        time_diff = end - start;
        time_old = time;
        time += (time_diff - time)/(i+1);
        var += (time_diff - time) * (time_diff - time_old);
    }
    printf("mean: %f\n", time);
    printf("stddev: %f\n", REPETITIONS<=1? 0: sqrt(var / (REPETITIONS - 1)));
}
