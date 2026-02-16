// transformation functions used in process model
real inv_tr_eta(real x) {
  return Phi_approx(x);
}
real tr_eta(real y) {
  return inv_Phi(y);
}

row_vector inv_tr_eta_vector(row_vector x) {
   return Phi_approx(x);
}

matrix inv_tr_eta_matrix(matrix x) {
   return Phi_approx(x);
}

row_vector tr_eta_vector(row_vector y) {
   return inv_Phi(y);
}

vector inv_tr_eta_colvector(vector x) {
//   return 0.001 + 0.998/(1+exp(-x));
//  return 0.001 + 0.998*Phi_approx(x);
 //  return 0.001 + 0.998*Phi(x);
   return Phi_approx(x);
}
vector tr_eta_colvector(vector y) {
//   return 0.001 + 0.998/(1+exp(-x));
   // no _approx version
//   return inv_Phi((y - 0.001)/0.998);
   // (y - ymin)/(ymax - ymin)
   return inv_Phi(y);
}
