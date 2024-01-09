#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <complex.h>

#define PI 3.14159265358979323846
#define WINDOW 256
#define BEAM 1024
#define RANGE 8

void transpose(double complex input[WINDOW][BEAM][RANGE], double complex output[BEAM][RANGE][WINDOW]);
void applyHanningWindow(float data[], size_t N);
void fft(double complex *X, int N);
void calculateEnvelope(double complex *X, float *envelope, int N);

void transpose(double complex input[WINDOW][BEAM][RANGE], double complex output[BEAM][RANGE][WINDOW]) {
    int z, y, x;
    for (z = 0; z < WINDOW; ++z) {
        for (y = 0; y < BEAM; ++y) {
            for (x = 0; x < RANGE; ++x) {
                output[y][x][z] = input[z][y][x];
            }
        }
    }
}

void applyHanningWindow(float data[], size_t N) {
    size_t i;
    for (i = 0; i < N; ++i) {
        data[i] *= 0.5 * (1 - cos(2 * PI * i / (N - 1)));
    }
}

void fft(double complex *X, int N) {
    int i, k;
    double complex t;
    double complex even[N / 2], odd[N / 2];

    if (N <= 1) return;

    for (i = 0; i < N / 2; i++) {
        even[i] = X[i * 2];
        odd[i] = X[i * 2 + 1];
    }

    fft(even, N / 2);
    fft(odd, N / 2);

    for (k = 0; k < N / 2; k++) {
        t = cexp(-2 * PI * k / N * I) * odd[k];
        X[k] = even[k] + t;
        X[k + N / 2] = even[k] - t;
    }
}

void calculateEnvelope(double complex *X, float *envelope, int N) {
    int i;
    for (i = 0; i < N; ++i) {
        envelope[i] = cabs(X[i]);
    }
}

int main() {
    FILE *file;
    double complex (*input)[BEAM][RANGE];
    double complex (*output)[RANGE][WINDOW];
    float *dataSlice;
    double complex *fftData;
    float *envelope;
    int z, y, x, i;

    input = malloc(WINDOW * sizeof(*input));
    output = malloc(BEAM * sizeof(*output));
    dataSlice = malloc(WINDOW * sizeof(float));
    fftData = malloc(WINDOW * sizeof(double complex));
    envelope = malloc(WINDOW * sizeof(float));

    if (!input || !output || !dataSlice || !fftData || !envelope) {
        fprintf(stderr, "Memory allocation failed\n");
        if (input) free(input);
        if (output) free(output);
        if (dataSlice) free(dataSlice);
        if (fftData) free(fftData);
        if (envelope) free(envelope);
        return 1;
    }

    file = fopen("C:\\Users\\Zhang Yaoyu\\Documents\\WeChat Files\\wxid_snzrzkzsc35d22\\FileStorage\\File\\2023-11\\PC_CS.csv", "r");
    if (file == NULL) {
        perror("Error opening file");
        free(input);
        free(output);
        free(dataSlice);
        free(fftData);
        free(envelope);
        return 1;
    }

    for (z = 0; z < WINDOW; ++z) {
        for (y = 0; y < BEAM; ++y) {
            for (x = 0; x < RANGE; ++x) {
                double realPart, imagPart;
                if (fscanf(file, "%lf %lf", &realPart, &imagPart) != 2) {
                    fprintf(stderr, "Error reading data at window %d, beam %d, range %d\n", z, y, x);
                    fclose(file);
                    free(input);
                    free(output);
                    free(dataSlice);
                    free(fftData);
                    free(envelope);
                    return 1;
                }
                input[z][y][x] = realPart + imagPart * I;
            }
        }
    }

    fclose(file);

    file = fopen("dfb_out.csv", "w");
    if (file == NULL) {
        perror("Error opening file");
        free(input);
        free(output);
        free(dataSlice);
        free(fftData);
        free(envelope);
        return 1;
    }

    for (y = 0; y < BEAM; ++y) {
        for (x = 0; x < RANGE; ++x) {
            for (z = 0; z < WINDOW; ++z) {
                dataSlice[z] = creal(input[z][y][x]);
            }

            applyHanningWindow(dataSlice, WINDOW);

            for (i = 0; i < WINDOW; ++i) {
                fftData[i] = dataSlice[i] + 0 * I;
            }
            fft(fftData, WINDOW);

            calculateEnvelope(fftData, envelope, WINDOW);

            for (i = 0; i < WINDOW; ++i) {
                fprintf(file, "%.15f ", envelope[i]);
            }
        }
    }

    fclose(file);

    free(input);
    free(output);
    free(dataSlice);
    free(fftData);
    free(envelope);

    return 0;
}
