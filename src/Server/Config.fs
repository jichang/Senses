module Config

type JWTConfig =
    { secret: string
      issuer: string }

let jwtConfig =
    { secret = "spadR2dre#u-ruBrE@TepA&*Uf@U"
      issuer = "senses.feblr.com" }
