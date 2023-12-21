## 4. Modele de Fisher

dt = 0.01 ; Tmax=50 ; dx = 1/400 ; D=0.05 ; r=2 ; K=0.9 
X=seq(-1,1,dx) 
Nh = length(X)
U0 = ifelse(X<0.25 & X>0, 1, 0)

f = function(u){
  u * (1+ r * dt * (1-u/K))
}

f(U0)
mu = D * dt / (dx**2)
colA = c(c(1+2*mu, -mu),rep(0, Nh-2),
         rep(c(c(-mu, 1+2*mu, -mu), rep(0, Nh-2)), Nh-2),
         c(-mu, 1+2*mu))

A = matrix(colA, Nh, Nh)
A_inv = solve(A)
dev.set(1)
U = U0
plot(x=X, y= U0, type='l', main="fisher", xlim=c(-1, 1), ylim=c(0,1))

for (k in 1:350){
  U = A_inv %*% f(U)
  U[1] = 0
  U[Nh] = 0
  plot(x=X, y= U, type='l', main="fisher", xlim=c(-1, 1), ylim=c(0,1))
  Sys.sleep(0.05)
}
éa&