namespace Migrations

open SimpleMigrations

[<Migration(20180721194106L, "Create Videos Table")>]
type CreateVideosTable() =
    inherit Migration()

    override __.Up() =
      base.Execute(@"CREATE TABLE IF NOT EXISTS senses.videos(
        id BIGSERIAL,
        resource_id BIGINT NOT NULL,
        uri TEXT NOT NULL,
        created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
        updated_at TIMESTAMP WITH TIME ZONE,
        removed_at TIMESTAMP WITH TIME ZONE,
        status INTEGER NOT NULL DEFAULT 0,
        CONSTRAINT videos_pkey PRIMARY KEY (id),
        CONSTRAINT videos_resource_id_fkey FOREIGN KEY (resource_id) REFERENCES senses.resources(id)
      )")

    override __.Down() =
      base.Execute(@"DROP TABLE senses.videos")
