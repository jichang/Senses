namespace Migrations

open SimpleMigrations

[<Migration(20180721175403L, "Create TaskTypes Table")>]
type CreateTaskTypesTable() =
    inherit Migration()

    override __.Up() =
      base.Execute(@"CREATE TABLE IF NOT EXISTS senses.task_types(
        id SERIAL,
        key VARCHAR NOT NULL,
        created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
        updated_at TIMESTAMP WITH TIME ZONE,
        removed_at TIMESTAMP WITH TIME ZONE,
        status INTEGER NOT NULL DEFAULT 0,
        CONSTRAINT tasks_pkey PRIMARY KEY (id)
      )")

    override __.Down() =
      base.Execute(@"DROP TABLE senses.task_types")
