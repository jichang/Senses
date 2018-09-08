namespace Migrations

open SimpleMigrations

[<Migration(20180721193206L, "Create Images Table")>]
type CreateImagesTable() =
    inherit Migration()

    override __.Up() =
      base.Execute(@"CREATE TABLE IF NOT EXISTS senses.images(
        id BIGSERIAL,
        resource_id BIGINT NOT NULL,
        uri TEXT NOT NULL,
        created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
        updated_at TIMESTAMP WITH TIME ZONE,
        removed_at TIMESTAMP WITH TIME ZONE,
        status INTEGER NOT NULL DEFAULT 0,
        CONSTRAINT images_pkey PRIMARY KEY (id),
        CONSTRAINT images_resource_id_fkey FOREIGN KEY (resource_id) REFERENCES senses.resources(id)
      )")

    override __.Down() =
      base.Execute(@"DROP TABLE senses.images")
