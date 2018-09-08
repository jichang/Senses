namespace Migrations

open SimpleMigrations

[<Migration(20180721143902L, "Create Users Table")>]
type CreateUsersTable() =
    inherit Migration()

    override __.Up() =
      base.Execute(@"CREATE TABLE IF NOT EXISTS senses.users(
        id BIGSERIAL,
        uuid uuid NOT NULL,
        created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
        updated_at TIMESTAMP WITH TIME ZONE,
        removed_at TIMESTAMP WITH TIME ZONE,
        status INTEGER NOT NULL DEFAULT 0,
        CONSTRAINT users_pkey PRIMARY KEY (id),
        CONSTRAINT users_uuid_uninue_key UNIQUE (uuid)
      )")

    override __.Down() =
      base.Execute(@"DROP TABLE senses.users")
