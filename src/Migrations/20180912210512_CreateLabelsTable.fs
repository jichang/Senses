namespace Migrations

open SimpleMigrations

[<Migration(20180918070512L, "Create Labels Table")>]
type CreateLabelsTable() =
    inherit Migration()

    override __.Up() =
      base.Execute(@"CREATE TABLE IF NOT EXISTS senses.labels(
        id SERIAL,
        user_id BIGINT NOT NULL,
        color CHAR(8) NOT NULL,
        title VARCHAR NOT NULL,
        created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
        updated_at TIMESTAMP WITH TIME ZONE,
        removed_at TIMESTAMP WITH TIME ZONE,
        status INTEGER NOT NULL DEFAULT 0,
        CONSTRAINT labels_pkey PRIMARY KEY (id),
        CONSTRAINT labels_title_color_uninue_key UNIQUE (title, color)
      )")

    override __.Down() =
      base.Execute(@"DROP TABLE senses.labels")
