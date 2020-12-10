from mpmath import *
mp.dps = 22; mp.pretty = True

real_part = [6.3e-02 for i in range(0,10)]
    
for i in range(10):
    real_part.append(6.3)
    
for i in range(10):
    real_part.append(6.3e02)
    
real_part.append(1)
real_part.append(5.5)
real_part.append(3.9e4)
real_part.append(1)


imag_part_sample = [1e-20, 1e-14, 1e-12, 1e-10, 1e-6, 1e-2, 1e1, 1.2e1, 1.5e1, 2e2]
imag_part = imag_part_sample*3
imag_part.append(1e-20)
imag_part.append(1e-14)
imag_part.append(1)
imag_part.append(2.8e4)


for rp, ip in zip(real_part, imag_part):
    z = rp+1j*ip
    res = mp.exp(-z**2)*mp.erfc(-1j*z)
    print(f"cmplx({mp.re(res)}_rkind ,{mp.im(res)}_rkind , kind = rkind), \t&")
    
print('----------------------------------------------------------')

for rp, ip in zip(real_part, imag_part):
    z = rp+1j*ip
    z2 = 1j*z
    res = 1 - mp.exp(-z2**2)*mp.erfc(-1j*z2)*mp.exp(-z**2)
    print(f"cmplx({mp.re(res)}_rkind ,{mp.im(res)}_rkind , kind = rkind), \t&")


# rp = 6.3e-02
# ip = 200
# z = rp+1j*ip
# res = res = mp.exp(-z**2)*mp.erfc(-1j*z)
# print(res)
    
    



