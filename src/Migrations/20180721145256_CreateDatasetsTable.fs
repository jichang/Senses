namespace Migrations

open SimpleMigrations

[<Migration(20180721145256L, "Create Datasets Table")>]
type CreateDatasetsTable() =
    inherit Migration()

    override __.Up() =
      base.Execute(@"CREATE TABLE IF NOT EXISTS senses.datasets(
        id BIGSERIAL,
        user_id BIGINT NOT NULL,
        title VARCHAR NOT NULL,
        created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
        updated_at TIMESTAMP WITH TIME ZONE,
        removed_at TIMESTAMP WITH TIME ZONE,
        status INTEGER NOT NULL DEFAULT 0,
        CONSTRAINT datasets_pkey PRIMARY KEY (id),
        CONSTRAINT datasets_user_id_title_uninue_key UNIQUE (user_id, title),
        CONSTRAINT datasets_user_id_fkey FOREIGN KEY (user_id) REFERENCES senses.users(id)
      )")

    override __.Down() =
      base.Execute(@"DROP TABLE senses.datasets")
