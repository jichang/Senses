namespace Migrations

open SimpleMigrations

[<Migration(20180721190325L, "Create TasksResources Table")>]
type CreateTasksResourcesTable() =
    inherit Migration()

    override __.Up() =
      base.Execute @"CREATE TABLE IF NOT EXISTS senses.tasks_resource_types(
        id SERIAL,
        task_type_id INTEGER NOT NULL,
        resource_type_id INTEGER NOT NULL,
        created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
        updated_at TIMESTAMP WITH TIME ZONE,
        removed_at TIMESTAMP WITH TIME ZONE,
        status INTEGER NOT NULL DEFAULT 0,
        CONSTRAINT tasks_resource_types_pkey PRIMARY KEY (id),
        CONSTRAINT tasks_resource_types_task_type_id_fkey FOREIGN KEY (task_type_id) REFERENCES senses.task_types(id),
        CONSTRAINT tasks_resource_types_resource_type_id_fkey FOREIGN KEY (resource_type_id) REFERENCES senses.resource_types(id),
        CONSTRAINT tasks_resource_types_task_type_resource_type_unique_key UNIQUE(resource_type_id, task_type_id)
      )"

    override __.Down() =
      base.Execute @"DROP TABLE senses.tasks_resource_types"
