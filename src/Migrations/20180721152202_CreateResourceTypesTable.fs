namespace Migrations

open SimpleMigrations

[<Migration(20180721152202L, "Create ResourceTypes Table")>]
type CreateResourceTypesTable() =
    inherit Migration()

    override __.Up() =
      base.Execute(@"CREATE TABLE IF NOT EXISTS senses.resource_types(
        id SERIAL,
        key VARCHAR NOT NULL,
        created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
        updated_at TIMESTAMP WITH TIME ZONE,
        removed_at TIMESTAMP WITH TIME ZONE,
        status INTEGER NOT NULL DEFAULT 0,
        CONSTRAINT resource_types_pkey PRIMARY KEY (id),
        CONSTRAINT resource_types_key_unique_key UNIQUE(key)
      )")

    override __.Down() =
      base.Execute(@"DROP TABLE senses.resource_types")
