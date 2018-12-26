# Senses

A prototype for dataset labelling build with [SAFE stack](https://safe-stack.github.io/docs/)

## How to run

1 install PostgreSQL, see [document](https://www.postgresql.org/download/)

2 install fake tools, See [document](https://fake.build/fake-gettingstarted.html)

3 clone repo

    git clone https://github.com/JacobChang/Senses.git

3 setup database

    fake build --target RunMigrations

4 run project

    fake build --target run
