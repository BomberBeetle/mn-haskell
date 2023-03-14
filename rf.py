def regula_falsi(fn, a, b, tol):
    while True:
        ck = (fn(b)*a - fn(a)*b) / (fn(b) - fn(a))
        if abs(ck-tol) < tol or fn(ck)==0:
            return ck
        else:
            if fn(a)*fn(ck) < 0:
                b = ck
            elif fn(b)*fn(ck) < 0:
                a = ck
            else:
                return "bruh"

def fn_dum(x):
    return x*x

