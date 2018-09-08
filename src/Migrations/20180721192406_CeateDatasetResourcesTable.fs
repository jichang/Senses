namespace Migrations

open SimpleMigrations

[<Migration(20180721192406L, "Create DatasetResources Table")>]
type CreateDatasetResourcesTable() =
    inherit Migration()

    override __.Up() =
      base.Execute(@"CREATE TABLE IF NOT EXISTS senses.dataset_resources(
        id SERIAL,
        dataset_id BIGINT NOT NULL,
        resource_id BIGINT NOT NULL,
        created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
        updated_at TIMESTAMP WITH TIME ZONE,
        removed_at TIMESTAMP WITH TIME ZONE,
        status INTEGER NOT NULL DEFAULT 0,
        CONSTRAINT dataset_resources_pkey PRIMARY KEY (id),
        CONSTRAINT dataset_resources_dataset_id_fkey FOREIGN KEY (dataset_id) REFERENCES senses.datasets(id),
        CONSTRAINT dataset_resources_resource_id_fkey FOREIGN KEY (resource_id) REFERENCES senses.resources(id)
      )")

    override __.Down() =
      base.Execute(@"DROP TABLE senses.dataset_resources")
