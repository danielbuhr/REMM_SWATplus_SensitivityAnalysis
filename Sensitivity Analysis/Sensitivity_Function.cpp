#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List Calc_Sort_C(NumericVector data, NumericMatrix density_total, int n, int m){
	
	Rcpp::Environment base("package:KernSmooth");
	Rcpp::Function bkde_r = base["bkde"];
	
	NumericMatrix Smj(m, 512);
	NumericVector Sm(m);
	NumericMatrix line_output(m, 512);
	
	NumericVector s_range(2);
	s_range(0) = min(data);
	s_range(1) = max(data);
	
	int n_sample = n / m;
	NumericVector sample(n_sample);
	double Sm_sum = 0;
	
	for (int i = 0; i < m; i++){
		for (int j = 0; j < n_sample; j++){
			sample(j) = data(i * n_sample + j);
		}
		
		List density_m = bkde_r(_["x"] = sample,
											_["gridsize"] = 512,
											_["range.x"] = s_range);
		
		NumericVector density_x = density_m[0];
		NumericVector density_y = density_m[1];
		//std::cout << density_x(0) << " " << density_y(0);
		
		for (int j = 0; j < 512; j++){
			Smj(i,j) = std::abs(density_total(j, 1) - density_y(j));
			line_output(i,j) = density_y(j);
		}
		for (int j = 0; j < 511; j++){
			Sm(i) += 0.5 * (Smj(i, j + 1) + Smj(i, j)) * 
			(density_total(j + 1, 0) - density_total(j, 0));
		}
		Sm_sum += Sm(i);
	}
	
	//double d = density_y(4);
	double d = 1 / (2.0 * n) * (n_sample) * Sm_sum;
	//std::cout << "d = " << d << "\n";
	//double d = Sm(0);
	
	List ret; ret["d"] = d; ret["lines"] = line_output;
	return ret;
		 

}