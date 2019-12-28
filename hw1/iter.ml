let rec iter (n, f) b =
    if n <= 0 then b
    else iter (n - 1, f) (f b)
