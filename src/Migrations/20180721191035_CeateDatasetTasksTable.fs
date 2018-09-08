namespace Migrations

open SimpleMigrations

[<Migration(20180721191035L, "Create DatasetTasks Table")>]
type CreateDataTasksTable() =
    inherit Migration()

    override __.Up() =
      base.Execute(@"CREATE TABLE IF NOT EXISTS senses.dataset_tasks(
        id BIGSERIAL,
        dataset_id BIGINT NOT NULL,
        task_type_id INTEGER NOT NULL,
        created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
        updated_at TIMESTAMP WITH TIME ZONE,
        removed_at TIMESTAMP WITH TIME ZONE,
        status INTEGER NOT NULL DEFAULT 0,
        CONSTRAINT dataset_tasks_pkey PRIMARY KEY (id),
        CONSTRAINT dataset_tasks_dataset_id_fkey FOREIGN KEY (dataset_id) REFERENCES senses.datasets(id),
        CONSTRAINT dataset_tasks_task_type_id_fkey FOREIGN KEY (task_type_id) REFERENCES senses.task_types(id),
        CONSTRAINT dataset_tasks_dataset_type_type_id_unique_key UNIQUE(dataset_id, task_type_id)
      )")

    override __.Down() =
      base.Execute(@"DROP TABLE senses.dataset_tasks")
