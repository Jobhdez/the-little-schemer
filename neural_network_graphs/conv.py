def conv(vec, vec2):
    t = (len(vec) + len(vec2)) - 1
    convolution = [None] * t
    for i in range(t):
        summation = 0
        for j in range(len(vec)):
            if i - j >= 0 and i - j < len(vec2):
                summation += vec[j] * vec2[i-j]
        convolution[i] = summation

    return convolution
