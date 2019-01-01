# Senses

A prototype for dataset labelling build with [SAFE stack](https://safe-stack.github.io/docs/)

## How to run

1 install PostgreSQL, see [document](https://www.postgresql.org/download/)

There are two ways to setup database before running database migrations

First, you can create a role `feblr` with password `feblr` and a database `feblr`

Second, you can update code at `src/Migrations/Progra.fs` and `src/Server/Database.fs` to
use a exist database.

2 install fake tools, See [document](https://fake.build/fake-gettingstarted.html)

3 clone repo

    git clone https://github.com/JacobChang/Senses.git

3 setup database

    fake build --target RunMigrations

4 run project

    fake build --target run
