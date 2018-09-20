namespace Shared

#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

open System

module rec Model =
    type ApiError =
        { code: string }

        static member Decoder : Decode.Decoder<ApiError> =
            Decode.object (fun get -> { code = get.Required.Field "code" Decode.string } )

        static member Encoder (errorResponse : ApiError) =
            Encode.object
                [ "code", Encode.string errorResponse.code ]

    type ModelCollection<'t> =
        { totalCount: int64
          items: 't list }

        static member Decoder (itemDecoder: Decode.Decoder<'t>) : Decode.Decoder<ModelCollection<'t>> =
            Decode.object (fun get ->
                { totalCount = get.Required.Field "totalCount" Decode.int64
                  items = get.Required.Field "items" (Decode.list itemDecoder) }
            )

        static member Encoder (itemEncoder: Encode.Encoder<'t>) (modelCollection: ModelCollection<'t>) =
            Encode.object
                [ "totalCount", Encode.int64 modelCollection.totalCount
                  "items", Encode.list (List.map itemEncoder modelCollection.items) ]

    type Session =
        { token: string }

        static member Decoder : Decode.Decoder<Session> =
            Decode.object
                (fun get ->
                    { token = get.Required.Field "token" Decode.string }
                )

        static member Encoder (session : Session) =
            Encode.object
                [ "token", Encode.string session.token ]

    type User =
        { id: int64
          uuid: Guid
          status: int32 }

        static member Decoder : Decode.Decoder<User> =
            Decode.object
                (fun get ->
                    { id = get.Required.Field "id" Decode.int64
                      uuid = get.Required.Field "uuid" Decode.guid
                      status = get.Required.Field "status" Decode.int }
                )

        static member Encoder (user : User) =
            Encode.object
                [ "id", Encode.int64 user.id
                  "uuid", Encode.guid user.uuid
                  "status", Encode.int user.status ]

    type DatasetCreateParams =
        { title: string }

        static member Decoder : Decode.Decoder<DatasetCreateParams> =
            Decode.object (fun get -> { title = get.Required.Field "title" Decode.string } )

        static member Encoder (datasetCreateParams: DatasetCreateParams) =
            Encode.object [ "title", Encode.string datasetCreateParams.title ]

    type Dataset =
        { id: int64
          user: User
          title: string
          tasks: ModelCollection<Task>
          resources: ModelCollection<Resource>
          status: int }

        static member Decoder : Decode.Decoder<Dataset> =
            Decode.object
                (fun get ->
                    { id = get.Required.Field "id" Decode.int64
                      user = get.Required.Field "user" User.Decoder
                      title = get.Required.Field "title" Decode.string
                      tasks = get.Required.Field "tasks" (ModelCollection.Decoder Task.Decoder)
                      resources = get.Required.Field "resources" (ModelCollection.Decoder Resource.Decoder)
                      status = get.Required.Field "status" Decode.int }
                )

        static member Encoder (dataset : Dataset) =
            Encode.object
                [ "id", Encode.int64 dataset.id
                  "user", User.Encoder dataset.user
                  "title", Encode.string dataset.title
                  "tasks", ModelCollection.Encoder Task.Encoder dataset.tasks
                  "resources", ModelCollection.Encoder Resource.Encoder dataset.resources
                  "status", Encode.int dataset.status ]

    type ResourceTypeKey =
        | Image
        | Text
        | Video

        static member Decoder : Decode.Decoder<ResourceTypeKey> =
            Decode.string
            |> Decode.andThen
                (function
                | "image" -> Decode.succeed ResourceTypeKey.Image
                | "text" -> Decode.succeed ResourceTypeKey.Text
                | "video" -> Decode.succeed ResourceTypeKey.Video
                | invalid -> Decode.fail (sprintf "Failed to decode `%s` it's an invalide case for `ResourceTypeKey`" invalid) )

        static member Encoder (resourceTypeKey : ResourceTypeKey) =
            match resourceTypeKey with
            | Image -> "image"
            | Text -> "text"
            | Video -> "video"
            |> Encode.string

    type ResourceType =
        { id: int
          key: ResourceTypeKey
          status: int }

        static member Decoder : Decode.Decoder<ResourceType> =
            Decode.object
                (fun get ->
                    { id = get.Required.Field "id" Decode.int
                      key = get.Required.Field "key" ResourceTypeKey.Decoder
                      status = get.Required.Field "status" Decode.int }
                )

        static member Encoder (resourceType : ResourceType) =
            Encode.object
                [ "id", Encode.int resourceType.id
                  "key", ResourceTypeKey.Encoder resourceType.key
                  "status", Encode.int resourceType.status ]

    type TaskTypeKey =
        | Label

        static member Decoder : Decode.Decoder<TaskTypeKey> =
            Decode.string
            |> Decode.andThen
                (function
                | "label" -> Decode.succeed TaskTypeKey.Label
                | invalid -> Decode.fail (sprintf "Failed to decode `%s` it's an invalide case for `TaskTypeKey`" invalid) )

        static member Encoder (taskTypeKey : TaskTypeKey) =
            match taskTypeKey with
            | Label -> "label"
            |> Encode.string

    type TaskType =
        { id: int
          key: TaskTypeKey
          status: int }

        static member Decoder : Decode.Decoder<TaskType> =
            Decode.object
                (fun get ->
                    { id = get.Required.Field "id" Decode.int
                      key = get.Required.Field "key" TaskTypeKey.Decoder
                      status = get.Required.Field "status" Decode.int }
                )

        static member Encoder (taskType : TaskType) =
            Encode.object
                [ "id", Encode.int taskType.id
                  "key", TaskTypeKey.Encoder taskType.key
                  "status", Encode.int taskType.status ]

    type Task =
        { id: int64
          ``type``: TaskType
          status: int }

        static member Decoder : Decode.Decoder<Task> =
            Decode.object
                (fun get ->
                    { id = get.Required.Field "id" Decode.int64
                      ``type`` = get.Required.Field "type" TaskType.Decoder
                      status = get.Required.Field "status" Decode.int }
                )

        static member Encoder (task: Task) =
            Encode.object
                [ "id", Encode.int64 task.id
                  "type", TaskType.Encoder task.``type``
                  "status", Encode.int task.status ]

    type Image =
        { id: int64
          uri: string // should use uri
          status: int }

        static member Decoder : Decode.Decoder<Image> =
            Decode.object
                (fun get ->
                    { id = get.Required.Field "id" Decode.int64
                      uri = get.Required.Field "uri" Decode.string
                      status = get.Required.Field "status" Decode.int }
                )

        static member Encoder (image : Image) =
            Encode.object
                [ "id", Encode.int64 image.id
                  "uri", Encode.string image.uri
                  "status", Encode.int image.status ]

    type Video =
        { id: int64
          uri: string // should use Uri
          status: int }

        static member Decoder : Decode.Decoder<Video> =
            Decode.object
                (fun get ->
                    { id = get.Required.Field "id" Decode.int64
                      uri = get.Required.Field "uri" Decode.string
                      status = get.Required.Field "status" Decode.int }
                )

        static member Encoder (video : Video) =
            Encode.object
                [ "id", Encode.int64 video.id
                  "uri", Encode.string video.uri
                  "status", Encode.int video.status ]

    type Txt =
        { id: int64
          content: string
          status: int }

        static member Decoder : Decode.Decoder<Txt> =
            Decode.object
                (fun get ->
                    { id = get.Required.Field "id" Decode.int64
                      content = get.Required.Field "content" Decode.string
                      status = get.Required.Field "status" Decode.int }
                )

        static member Encoder (text : Txt) =
            Encode.object
                [ "id", Encode.int64 text.id
                  "content", Encode.string text.content
                  "status", Encode.int text.status ]

    type ResourceContent =
        | Txt of Txt
        | Video of Video
        | Image of Image

        static member Decoder : Decode.Decoder<ResourceContent> =
            Decode.object (fun get ->
                match get.Required.Field "type" ResourceTypeKey.Decoder with
                | ResourceTypeKey.Image ->
                    get.Required.Field "image" (Image.Decoder |> Decode.map Image)
                | ResourceTypeKey.Video ->
                    get.Required.Field "video" (Video.Decoder |> Decode.map Video)
                | ResourceTypeKey.Text ->
                    get.Required.Field "text" (Txt.Decoder |> Decode.map Txt)
                )

        static member Encoder (resourceContent: ResourceContent) =
            match resourceContent with
            | Txt text ->
                Encode.object
                    [ "type", ResourceTypeKey.Encoder ResourceTypeKey.Text
                      "text", Txt.Encoder text ]
            | Video video ->
                Encode.object
                    [ "type", ResourceTypeKey.Encoder ResourceTypeKey.Text
                      "video", Video.Encoder video ]
            | Image image ->
                Encode.object
                    [ "type", ResourceTypeKey.Encoder ResourceTypeKey.Text
                      "image", Image.Encoder image ]

    type Resource =
        { id: int64
          content: ResourceContent
          status: int }

        static member Decoder : Decode.Decoder<Resource> =
            Decode.object
                (fun get ->
                    { id = get.Required.Field "id" Decode.int64
                      content = get.Required.Field "content" ResourceContent.Decoder
                      status = get.Required.Field "status" Decode.int }
                )

        static member Encoder (resource : Resource) =
            Encode.object
                [ "id", Encode.int64 resource.id
                  "content", ResourceContent.Encoder resource.content
                  "status", Encode.int resource.status ]

    type LabelCreateParams =
        { color: string
          title: string }

        static member Decoder : Decode.Decoder<LabelCreateParams> =
            Decode.object (fun get ->
                { color = get.Required.Field "color" Decode.string
                  title = get.Required.Field "title" Decode.string } )

        static member Encoder (datasetCreateParams: LabelCreateParams) =
            Encode.object
                [ "color", Encode.string datasetCreateParams.color
                  "title", Encode.string datasetCreateParams.title ]

    type Label =
        { id: int
          color: string
          title: string 
          status: int }
    
        static member Decoder : Decode.Decoder<Label> =
            Decode.object
                (fun get ->
                    { id = get.Required.Field "id" Decode.int
                      color = get.Required.Field "color" Decode.string
                      title = get.Required.Field "title" Decode.string
                      status = get.Required.Field "status" Decode.int }
                )

        static member Encoder (label : Label) =
            Encode.object
                [ "id", Encode.int label.id
                  "color", Encode.string label.color
                  "title", Encode.string label.title
                  "status", Encode.int label.status ]