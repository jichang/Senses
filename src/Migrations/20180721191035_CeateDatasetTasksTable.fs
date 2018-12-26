namespace Migrations

open SimpleMigrations

[<Migration(20180721191035L, "Create DatasetTasks Table")>]
type CreateDatasetTasksTable() =
    inherit Migration()

    override __.Up() =
      base.Execute(@"CREATE TABLE IF NOT EXISTS senses.dataset_tasks(
        id BIGSERIAL,
        task_type_id INTEGER NOT NULL,
        dataset_slice_id BIGINT NOT NULL,
        created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
        updated_at TIMESTAMP WITH TIME ZONE,
        removed_at TIMESTAMP WITH TIME ZONE,
        status INTEGER NOT NULL DEFAULT 0,
        CONSTRAINT dataset_tasks_pkey PRIMARY KEY (id),
        CONSTRAINT dataset_tasks_task_type_id_fkey FOREIGN KEY (task_type_id) REFERENCES senses.task_types(id),
        CONSTRAINT dataset_tasks_dataset_slice_id_fkey FOREIGN KEY (dataset_slice_id) REFERENCES senses.dataset_slices(id)
      )")

    override __.Down() =
      base.Execute(@"DROP TABLE senses.dataset_tasks")
