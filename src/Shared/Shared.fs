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

    type Summary =
        { datasetsCount: int64
          labelsCount: int64 }

        static member Decoder : Decode.Decoder<Summary> =
            Decode.object
                (fun get ->
                    { datasetsCount = get.Required.Field "datasetsCount" Decode.int64
                      labelsCount = get.Required.Field "labelsCount" Decode.int64 }
                )

        static member Encoder (summary : Summary) =
            Encode.object
                [ "datasetsCount", Encode.int64 summary.datasetsCount
                  "labelsCount", Encode.int64 summary.labelsCount ]

    type DatasetSlice =
        { id: int64
          title: string
          status: int }

        static member Decoder : Decode.Decoder<DatasetSlice> =
            Decode.object
                (fun get ->
                    { id = get.Required.Field "id" Decode.int64
                      title = get.Required.Field "title" Decode.string
                      status = get.Required.Field "status" Decode.int }
                )

        static member Encoder (dataset : DatasetSlice) =
            Encode.object
                [ "id", Encode.int64 dataset.id
                  "title", Encode.string dataset.title
                  "status", Encode.int dataset.status ]

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
          slices: ModelCollection<DatasetSlice>
          status: int }

        static member Decoder : Decode.Decoder<Dataset> =
            Decode.object
                (fun get ->
                    { id = get.Required.Field "id" Decode.int64
                      user = get.Required.Field "user" User.Decoder
                      title = get.Required.Field "title" Decode.string
                      tasks = get.Required.Field "tasks" (ModelCollection.Decoder Task.Decoder)
                      slices = get.Required.Field "slices" (ModelCollection.Decoder DatasetSlice.Decoder)
                      status = get.Required.Field "status" Decode.int }
                )

        static member Encoder (dataset : Dataset) =
            Encode.object
                [ "id", Encode.int64 dataset.id
                  "user", User.Encoder dataset.user
                  "title", Encode.string dataset.title
                  "tasks", ModelCollection.Encoder Task.Encoder dataset.tasks
                  "slices", ModelCollection.Encoder DatasetSlice.Encoder dataset.slices
                  "status", Encode.int dataset.status ]

    type ResourceTypeKey =
        | Image
        | Text
        | Video

        static member Decoder : Decode.Decoder<ResourceTypeKey> =
            Decode.string
            |> Decode.andThen
                (function
                | "Image" -> Decode.succeed ResourceTypeKey.Image
                | "Text" -> Decode.succeed ResourceTypeKey.Text
                | "Video" -> Decode.succeed ResourceTypeKey.Video
                | invalid -> Decode.fail (sprintf "Failed to decode `%s` it's an invalide case for `ResourceTypeKey`" invalid) )

        static member Encoder (resourceTypeKey : ResourceTypeKey) =
            match resourceTypeKey with
            | Image -> "Image"
            | Text -> "Text"
            | Video -> "Video"
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
                | "Label" -> Decode.succeed TaskTypeKey.Label
                | invalid -> Decode.fail (sprintf "Failed to decode `%s` it's an invalide case for `TaskTypeKey`" invalid) )

        static member Encoder (taskTypeKey : TaskTypeKey) =
            match taskTypeKey with
            | Label -> "Label" |> Encode.string

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
          labels: ModelCollection<Label>
          datasetSlices: ModelCollection<DatasetSlice>
          status: int }

        static member Decoder : Decode.Decoder<Task> =
            Decode.object
                (fun get ->
                    { id = get.Required.Field "id" Decode.int64
                      ``type`` = get.Required.Field "type" TaskType.Decoder
                      labels = get.Required.Field "labels" (ModelCollection.Decoder Label.Decoder)
                      datasetSlices = get.Required.Field "datasetSlices" (ModelCollection.Decoder DatasetSlice.Decoder)
                      status = get.Required.Field "status" Decode.int }
                )

        static member Encoder (task: Task) =
            Encode.object
                [ "id", Encode.int64 task.id
                  "type", TaskType.Encoder task.``type``
                  "labels", (ModelCollection.Encoder Label.Encoder) task.labels
                  "datasetSlices", (ModelCollection.Encoder DatasetSlice.Encoder) task.datasetSlices
                  "status", Encode.int task.status ]

    type TaskCreateParams =
        { taskType: TaskType
          labels: Label list
          datasetSlices: DatasetSlice list }

        static member Decoder : Decode.Decoder<TaskCreateParams> =
             Decode.object
                 (fun get ->
                     { taskType = get.Required.Field "taskType" TaskType.Decoder
                       labels = get.Required.Field "labels" (Decode.list Label.Decoder)
                       datasetSlices = get.Required.Field "datasetSlices" (Decode.list DatasetSlice.Decoder) } )

        static member Encoder (taskCreateParams : TaskCreateParams) =
            Encode.object
                [ "taskType", TaskType.Encoder taskCreateParams.taskType
                  "labels", Encode.list (List.map Label.Encoder taskCreateParams.labels)
                  "datasetSlices", Encode.list (List.map DatasetSlice.Encoder taskCreateParams.datasetSlices) ]

    type Resource =
        { id: int64
          ``type``: ResourceType
          uri: string
          content: string
          status: int }

        static member Decoder : Decode.Decoder<Resource> =
            Decode.object
                (fun get ->
                    { id = get.Required.Field "id" Decode.int64
                      ``type`` = get.Required.Field "type" ResourceType.Decoder
                      uri = get.Required.Field "uri" Decode.string
                      content = get.Required.Field "content" Decode.string
                      status = get.Required.Field "status" Decode.int }
                )

        static member Encoder (resource : Resource) =
            Encode.object
                [ "id", Encode.int64 resource.id
                  "type", ResourceType.Encoder resource.``type``
                  "uri", Encode.string resource.uri
                  "content", Encode.string resource.content
                  "status", Encode.int resource.status ]

    type ResourceCreateResponse =
        { total: int64 }

        static member Decoder : Decode.Decoder<ResourceCreateResponse> =
            Decode.object
                (fun get ->
                    { total = get.Required.Field "id" Decode.int64 }
                )

        static member Encoder (resource : ResourceCreateResponse) =
            Encode.object
                [ "total", Encode.int64 resource.total ]

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

    type ResourceSource =
        { uri: string
          content: string }

        static member Decoder : Decode.Decoder<ResourceSource> =
            Decode.object
                (fun get ->
                    { uri = get.Required.Field "uri" Decode.string
                      content = get.Required.Field "content" Decode.string }
                )

        static member Encoder (source : ResourceSource) =
            Encode.object
                [ "uri", Encode.string source.uri
                  "content", Encode.string source.content ]
 
    type ResourceFileLine =
        { version: string
          ``type``: ResourceTypeKey
          source: ResourceSource
          }

        static member Decoder : Decode.Decoder<ResourceFileLine> =
            Decode.object
                (fun get ->
                    { version = get.Required.Field "version" Decode.string
                      ``type`` = get.Required.Field "type" ResourceTypeKey.Decoder
                      source = get.Required.Field "source" ResourceSource.Decoder }
                )

        static member Encoder (fileLine : ResourceFileLine) =
            Encode.object
                [ "version", Encode.string fileLine.version
                  "type", ResourceTypeKey.Encoder fileLine.``type``
                  "source", ResourceSource.Encoder fileLine.source ]

    type Point =
        { x: float
          y: float }

    type Circle =
        { radius: float
          center: Point }

    type Rectangle =
        { origin: Point
          width: float
          height: float }

    type Polygon =
        { points: Point list }

    type Shape =
        | Point of Point
        | Circle of Circle
        | Rectangle of Rectangle
        | Polygon of Polygon

    type ResourceLabel =
        { label: Label
          shape: Shape }
