namespace Migrations

open SimpleMigrations

[<Migration(20180721190625L, "Create TasksResources Rows")>]
type CreateTasksResourcesRows() =
    inherit Migration()

    override __.Up() =
      base.Execute """
        INSERT INTO senses.tasks_resource_types(task_type_id, resource_type_id) (
          SELECT task_types.id, resource_types.id
          FROM senses.task_types
          CROSS JOIN senses.resource_types
          WHERE senses.task_types.key = 'label' AND senses.resource_types.key IN ('image', 'video', 'text')
        )
      """

    override __.Down() =
      base.Execute """
        DELETE FROM senses.tasks_resources
        LEFT JOIN senses.task_types ON tasks_resources.task_type_id = senses.task_types.id
        LEFT JOIN senses.resource_types ON tasks_resources.resource_type_id = senses.resource_types.id
        WHERE senses.task_types.key = 'label' AND senses.resource_types.key IN ('image', 'video', 'text')
      """
