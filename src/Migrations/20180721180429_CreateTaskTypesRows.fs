namespace Migrations

open SimpleMigrations

[<Migration(20180721180429L, "Create TaskTypes Rows")>]
type CreateTaskTypesRows() =
    inherit Migration()

    override __.Up() =
      base.Execute """
        INSERT INTO senses.task_types(key)
        VALUES ('label');
      """

    override __.Down() =
      base.Execute """
        DELETE FROM senses.task_types
        WHERE key IN ('label')
      """
