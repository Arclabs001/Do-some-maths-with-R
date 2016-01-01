compute_error <- function(m, b, data)
{
    r <- nrow(data)
    error <- 0
    for(i in 1:r){
        error <- error + (m * data[i,1] + b - data[i,2])^2
    }
    as.numeric(error/r)
}

step_gredient <- function(m_cur, b_cur, data, learningRate)
{
    m_gradient <- 0
    b_gradient <- 0
    N <- as.numeric(nrow(data))
    for(i in 1:nrow(data)){
        x <- data[i, 1]
        y <- data[i, 2]
        m_gradient <- m_gradient + -(2/N)*x*(y - (m_cur*x + b_cur))
        b_gradient <- b_gradient + -(2/N)*(y - (m_cur*x + b_cur))
    }
    new_m <- m_cur - learningRate * m_gradient
    new_b <- b_cur - learningRate * b_gradient
    as.numeric(c(new_m, new_b))
}

gradient_decent_runner <- function(data, starting_m,
                                   starting_b, learningRate,
                                   iterations)
{
    m <- starting_m
    b <- starting_b
    for(i in 1:iterations){
        v <- step_gredient(m, b, data, learningRate)
        m <- v[1]
        b <- v[2]
    }
    as.numeric(c(m, b))
}

gradient_decent_start <- function(data){
    learningRate <- 0.0001
    starting_m <- 0
    starting_b <- 0
    num_iterations <- 1000
    print(paste("Starting gradient descent at m =", 
                starting_m,", b =", starting_b, ", error =", 
                compute_error(starting_m, starting_b, data)))
    print("Running...")
    v <- gradient_decent_runner(data, starting_m, starting_b, 
                                learningRate, num_iterations)
    print(paste("After ", num_iterations, " iterations m =",
                v[1], ", b =", v[2], ", error =",
                compute_error(v[1], v[2], data)))
    
}