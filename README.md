# Study Erlang

### Calculate Pi using using Monte Carlo method
- `pi_seq`: Implements uses sequential calculations
- `pi_con`: Implements uses concurrency

### Test
- Compile:
    ```shell
    c(pi_seq).
    c(pi_con).
    ```
- Run and compare elapsed time:
    ```shell
    pi_seq:run(23000000, 30).
    ```
    ```console
    Pi: 3.1411624721233706
    Elapsed time: 65.489 seconds
    ```
    
    ```shell
    pi_con:run(23000000, 30).
    ```
    ```console
    Pi: 3.1411339503854805
    Elapsed time: 19.501 seconds
    ```
