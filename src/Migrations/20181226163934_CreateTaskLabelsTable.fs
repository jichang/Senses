namespace Migrations

open SimpleMigrations

[<Migration(20181226163934L, "Create TaskLabels Table")>]
type CreateTaskLabelsTable() =
    inherit Migration()

    override __.Up() =
      base.Execute(@"CREATE TABLE IF NOT EXISTS senses.task_labels(
        id BIGSERIAL,
        dataset_task_id BIGINT NOT NULL,
        label_id INTEGER NOT NULL,
        created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
        updated_at TIMESTAMP WITH TIME ZONE,
        removed_at TIMESTAMP WITH TIME ZONE,
        status INTEGER NOT NULL DEFAULT 0,
        CONSTRAINT task_labels_pkey PRIMARY KEY (id),
        CONSTRAINT task_labels_dataset_task_id_fkey FOREIGN KEY (dataset_task_id) REFERENCES senses.dataset_tasks(id),
        CONSTRAINT task_labels_label_id_fkey FOREIGN KEY (label_id) REFERENCES senses.labels(id),
        CONSTRAINT task_labels_dataset_task_id_label_id_unique_key UNIQUE(dataset_task_id, label_id)
      )")

    override __.Down() =
      base.Execute(@"DROP TABLE senses.task_labels")
