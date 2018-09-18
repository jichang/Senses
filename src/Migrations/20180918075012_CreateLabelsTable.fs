namespace Migrations

open SimpleMigrations

[<Migration(20180721191907L, "Create Labels Table")>]
type CreateLabelsTable() =
    inherit Migration()

    override __.Up() =
      base.Execute(@"CREATE TABLE IF NOT EXISTS senses.labels(
        id SERIAL,
        color CHAR(8) NOT NULL,
        title VARCHAR NOT NULL,
        created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
        updated_at TIMESTAMP WITH TIME ZONE,
        removed_at TIMESTAMP WITH TIME ZONE,
        status INTEGER NOT NULL DEFAULT 0,
        CONSTRAINT resources_pkey PRIMARY KEY (id),
        CONSTRAINT resources_resource_type_id_fkey FOREIGN KEY (resource_type_id) REFERENCES senses.resource_types(id)
      )")

    override __.Down() =
      base.Execute(@"DROP TABLE senses.labels")
