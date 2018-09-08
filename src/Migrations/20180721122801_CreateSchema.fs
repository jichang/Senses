namespace Migrations

open SimpleMigrations

[<Migration(20180721122801L, "Create Schema")>]
type CreateSchema() =
    inherit Migration()

    override __.Up() =
      base.Execute(@"CREATE SCHEMA IF NOT EXISTS senses")

    override __.Down() =
      base.Execute(@"DROP SCHEMA senses")
