// #include <Rcpp.h>
// #include <cuda_runtime.h>
// #include <cufft.h>
//
// using namespace Rcpp;
//
// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
// //' @name cufft
// //' @title Perform a 1D FFT using CUFFT
// //' @description This function uses CUDA's CUFFT library to compute the 1D FFT on a given set of data.
// //' @param n Integer. The number of elements in the FFT.
// //' @param inverse Logical. Whether to compute the inverse FFT (TRUE) or forward FFT (FALSE).
// //' @param h_idata_re Numeric vector. The real part of the input data.
// //' @param h_idata_im Numeric vector. The imaginary part of the input data.
// //' @param h_odata_re Numeric vector. The real part of the output data.
// //' @param h_odata_im Numeric vector. The imaginary part of the output data.
// //' @return A list with the real and imaginary parts of the transformed data.
// //' @export
// /* this fixes it */
// //[[Rcpp::export]]
//  void cufft(Rcpp::IntegerVector n,
//             bool inverse,
//             Rcpp::NumericVector h_idata_re,
//             Rcpp::NumericVector h_idata_im,
//             Rcpp::NumericVector h_odata_re,
//             Rcpp::NumericVector h_odata_im) {
//
//    int len = n[0];  // Get the number of elements in the FFT
//    cufftHandle plan;
//
//    // Create host data array for complex numbers
//    std::vector<cufftDoubleComplex> h_data(len);
//
//    // Convert input data to complex format
//    for (int i = 0; i < len; ++i) {
//      h_data[i].x = h_idata_re[i];
//      h_data[i].y = h_idata_im[i];
//    }
//
//    // Allocate device memory
//    cufftDoubleComplex* d_data;
//    cudaError_t err = cudaMalloc(&d_data, sizeof(cufftDoubleComplex) * len);
//    if (err != cudaSuccess) {
//      Rcpp::stop("CUDA malloc failed: %s", cudaGetErrorString(err));
//    }
//
//    // Copy data from host to device
//    cudaMemcpy(d_data, h_data.data(), sizeof(cufftDoubleComplex) * len, cudaMemcpyHostToDevice);
//
//    // Create FFT plan and execute
//    cufftPlan1d(&plan, len, CUFFT_Z2Z, 1);
//    cufftExecZ2Z(plan, d_data, d_data, inverse ? CUFFT_INVERSE : CUFFT_FORWARD);
//
//    // // Display results
//    // for (int i = 0; i < len; ++i) {
//    //   std::cout << "data[" << i << "] = (" << h_data[i].x << ", " << h_data[i].y << ")\n";
//    // }
//
//
//    // Copy results back from device to host
//    cudaMemcpy(h_data.data(), d_data, sizeof(cufftDoubleComplex) * len, cudaMemcpyDeviceToHost);
//
//    // Split the result into real and imaginary parts
//    for (int i = 0; i < len; ++i) {
//      h_odata_re[i] = h_data[i].x;
//      h_odata_im[i] = h_data[i].y;
//    }
//
//    // Cleanup
//    cufftDestroy(plan);
//    cudaFree(d_data);
//  }
//
// /*** R
// library(slvwagner)
// GPU_fft1D <- function(n ,  inverse ,  h_idata_re ,  h_idata_im ,  h_odata_re ,  h_odata_im){
//   cufft(n ,  inverse ,  h_idata_re ,  h_idata_im ,  h_odata_re ,  h_odata_im )
//   return(complex(real = h_odata_re, imaginary = h_odata_im))
// }
//
// set.seed(5)
// num <- 1e7
// z <- complex(real = stats::rnorm(num), imaginary = stats::rnorm(num))
// n <- length(z)
// inverse <- FALSE
// h_idata_re <- as.double(Re(z))
// h_idata_im <- as.double(Im(z))
// h_odata_re <- double(length=n)
// h_odata_im <- double(length=n)
//
//
// cufft(n ,  inverse ,  h_idata_re ,  h_idata_im ,  h_odata_re ,  h_odata_im )
// complex(real = h_odata_re, imaginary = h_odata_im)
//
// c_Digit <- 6
// (round(GPU_fft1D(n ,  inverse ,  h_idata_re ,  h_idata_im ,  h_odata_re ,  h_odata_im ),c_Digit)|>sum()) == (round(fft(z),c_Digit)|>sum())
//
// result <- microbenchmark::microbenchmark(
//   "GPU FFT" = cufft(n ,  inverse ,  h_idata_re ,  h_idata_im ,  h_odata_re ,  h_odata_im ),
//   "GPU_fft1D" = GPU_fft1D(n ,  inverse ,  h_idata_re ,  h_idata_im ,  h_odata_re ,  h_odata_im ),
//   "CPU FFT" = fft(z),
//   times = 100
// )
// result
// plot(result)
//
// */
