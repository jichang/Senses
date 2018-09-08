namespace Migrations

open SimpleMigrations

[<Migration(20180721153202L, "Create ResourceTypes Rows")>]
type CreateResourceTypesRows() =
    inherit Migration()

    override __.Up() =
      base.Execute @"
        INSERT INTO senses.resource_types(key)
        VALUES
          ('image'),
          ('video'),
          ('text')
        "

    override __.Down() =
      base.Execute """
        DELETE FROM senses.resource_types
        WHERE key IN ('image', 'video', 'text')
      """
