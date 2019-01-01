namespace Migrations

open SimpleMigrations

[<Migration(20181231112512L, "Create Resource Labels Table")>]
type CreateResourceLabelsTable() =
    inherit Migration()

    override __.Up() =
      base.Execute(@"CREATE TABLE IF NOT EXISTS senses.resource_labels(
        id BIGSERIAL,
        user_id BIGINT NOT NULL,
        resource_id BIGINT NOT NULL,
        label_id INTEGER NOT NULL,
        shape json NOT NULL,
        created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
        updated_at TIMESTAMP WITH TIME ZONE,
        removed_at TIMESTAMP WITH TIME ZONE,
        status INTEGER NOT NULL DEFAULT 0,
        CONSTRAINT resource_labels_pkey PRIMARY KEY (id),
        CONSTRAINT resource_labels_user_id_fkey FOREIGN KEY (user_id) REFERENCES senses.users(id),
        CONSTRAINT resource_labels_resource_id_fkey FOREIGN KEY (resource_id) REFERENCES senses.resources(id),
        CONSTRAINT resource_labels_label_id_fkey FOREIGN KEY (label_id) REFERENCES senses.labels(id)
      )")

    override __.Down() =
      base.Execute(@"DROP TABLE senses.resource_labels")
