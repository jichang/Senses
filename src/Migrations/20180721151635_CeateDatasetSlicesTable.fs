namespace Migrations

open SimpleMigrations

[<Migration(20180721151635L, "Create DatasetSlices Table")>]
type CreateDatasetSlicesTable() =
    inherit Migration()

    override __.Up() =
      base.Execute(@"CREATE TABLE IF NOT EXISTS senses.dataset_slices(
        id BIGSERIAL,
        user_id BIGINT NOT NULL,
        dataset_id BIGINT NOT NULL,
        title VARCHAR NOT NULL,
        created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
        updated_at TIMESTAMP WITH TIME ZONE,
        removed_at TIMESTAMP WITH TIME ZONE,
        status INTEGER NOT NULL DEFAULT 0,
        CONSTRAINT dataset_slices_pkey PRIMARY KEY (id),
        CONSTRAINT dataset_slices_user_id_fkey FOREIGN KEY (user_id) REFERENCES senses.users(id),
        CONSTRAINT dataset_slices_dataset_id_fkey FOREIGN KEY (dataset_id) REFERENCES senses.datasets(id)
      )")

    override __.Down() =
      base.Execute(@"DROP TABLE senses.dataset_slices")
