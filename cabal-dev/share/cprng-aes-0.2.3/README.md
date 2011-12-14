CPRNG-AES
=========

This module provides a crypto pseudo random number generator using AES in counter mode.

to import:

    import Crypto.Random.AESCtr

to use:

    rng <- makeSystem
    let (ran, rng') = getRandomBytes rng 1024

it's also an instance of CryptoRandomGen from the crypto-api package.

Statistics
----------

Using ent, a randomness property maker on one 1Mb sample.

cprng-AES:

    Entropy = 7.999837 bits per byte.
    Optimum compression would reduce the size of this 1048576 byte file by 0 percent.
    Chi square distribution for 1048576 samples is 237.02.
    Arithmetic mean value of data bytes is 127.3422 (127.5 = random).
    Monte Carlo value for Pi is 3.143589568 (error 0.06 percent).

Compared to urandom with the same sampling:

    Entropy = 7.999831 bits per byte.
    Optimum compression would reduce the size of this 1048576 byte file by 0 percent.
    Chi square distribution for 1048576 samples is 246.63.
    Arithmetic mean value of data bytes is 127.6347 (127.5 = random).
    Monte Carlo value for Pi is 3.132465868 (error 0.29 percent).

