namespace Migrations

open SimpleMigrations

[<Migration(20181226165634L, "Create TaskDatasetSlices Table")>]
type CreateTaskDatasetSlicesTable() =
    inherit Migration()

    override __.Up() =
      base.Execute(@"CREATE TABLE IF NOT EXISTS senses.task_dataset_slices(
        id BIGSERIAL,
        dataset_task_id INTEGER NOT NULL,
        dataset_slice_id BIGINT NOT NULL,
        created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
        updated_at TIMESTAMP WITH TIME ZONE,
        removed_at TIMESTAMP WITH TIME ZONE,
        status INTEGER NOT NULL DEFAULT 0,
        CONSTRAINT task_dataset_slices_pkey PRIMARY KEY (id),
        CONSTRAINT task_dataset_slices_dataset_task_id_fkey FOREIGN KEY (dataset_task_id) REFERENCES senses.dataset_tasks(id),
        CONSTRAINT task_dataset_slices_dataset_slice_id_fkey FOREIGN KEY (dataset_slice_id) REFERENCES senses.dataset_slices(id),
        CONSTRAINT task_dataset_slices_dataset_task_id_dataset_slice_id_unique_key UNIQUE(dataset_task_id, dataset_slice_id)
      )")

    override __.Down() =
      base.Execute(@"DROP TABLE senses.task_dataset_slices")
