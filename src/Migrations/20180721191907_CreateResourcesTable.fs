namespace Migrations

open SimpleMigrations

[<Migration(20180721191907L, "Create Resources Table")>]
type CreateResourcesTable() =
    inherit Migration()

    override __.Up() =
      base.Execute(@"CREATE TABLE IF NOT EXISTS senses.resources(
        id BIGSERIAL,
        dataset_slice_id BIGINT NOT NULL,
        resource_type_id INTEGER NOT NULL,
        uri VARCHAR,
        content TEXT,
        created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
        updated_at TIMESTAMP WITH TIME ZONE,
        removed_at TIMESTAMP WITH TIME ZONE,
        status INTEGER NOT NULL DEFAULT 0,
        CONSTRAINT resources_pkey PRIMARY KEY (id),
        CONSTRAINT resources_dataset_slice_id_fkey FOREIGN KEY (dataset_slice_id) REFERENCES senses.dataset_slices(id),
        CONSTRAINT resources_resource_type_id_fkey FOREIGN KEY (resource_type_id) REFERENCES senses.resource_types(id)
      )")

    override __.Down() =
      base.Execute(@"DROP TABLE senses.resources")
