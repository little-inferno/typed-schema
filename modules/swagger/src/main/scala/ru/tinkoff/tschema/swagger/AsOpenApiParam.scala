package ru.tinkoff.tschema.swagger
import cats.data.NonEmptyList
import derevo.Derivation
import magnolia.{CaseClass, Magnolia, SealedTrait}

sealed trait AsOpenApiParam[T] {
  def types: Map[String, DescribedType]
  def optional(implicit cfg: AsOpenApiParam.Config): AsOpenApiParam[Option[T]]
}

object AsOpenApiParam extends AsOpenParamInstances[AsOpenApiParam] with Derivation[AsOpenApiParam] {
  type Typeclass[x] = AsOpenApiParam[x]

  def apply[T](param: AsOpenApiParam[T]): AsOpenApiParam[T] = param

  def combine[T](ctx: CaseClass[Typeclass, T])(implicit cfg:AsOpenApiParam.Config = AsOpenApiParam.defaultConfig): Typeclass[T] =
    AsMultiOpenApiParam[T](
      NonEmptyList
        .fromListUnsafe(ctx.parameters.toList)
        .flatMap { param =>
          param.typeclass match {
            case AsSingleOpenApiParam(t, r) => NonEmptyList.of(OpenApiParamField(param.label, t, cfg.copy(required = r.required)))
            case AsMultiOpenApiParam(ps)    => ps
          }
        }
    )

  def generate[T]: Typeclass[T] = macro Magnolia.gen[T]
  def instance[T]: Typeclass[T] = macro Magnolia.gen[T]

  case class Config(
      required: Boolean = true,
      style: Option[OpenApiParam.Style] = None,
      explode: Option[Boolean] = None
  )

  val defaultConfig = Config()
}

trait OpenApiParamInfo {
  def typ: SwaggerType
  def cfg: AsOpenApiParam.Config
  def types = typ.collectTypes
}
final case class OpenApiParamField(
    name: String,
    typ: SwaggerType,
    cfg: AsOpenApiParam.Config = AsOpenApiParam.defaultConfig
) extends OpenApiParamInfo

final case class AsMultiOpenApiParam[T](fields: NonEmptyList[OpenApiParamField]) extends AsOpenApiParam[T] {
  def parts: NonEmptyList[String]                                                                                  = fields.map(_.name)
  def types                                                                                                        = fields.foldLeft(Map.empty[String, DescribedType])(_ ++ _.types)
  def optional(implicit cfg: AsOpenApiParam.Config = AsOpenApiParam.defaultConfig): AsMultiOpenApiParam[Option[T]] =
    AsMultiOpenApiParam(
      fields.map(_.copy(cfg = cfg.copy(required = false)))
    )
}
final case class AsSingleOpenApiParam[T](typ: SwaggerType, cfg: AsOpenApiParam.Config = AsOpenApiParam.defaultConfig)
    extends AsOpenApiParam[T] with OpenApiParamInfo {
  def optional(implicit cfg: AsOpenApiParam.Config = AsOpenApiParam.defaultConfig): AsOpenApiParam[Option[T]] =
    AsSingleOpenApiParam(typ, cfg.copy(required = false))
}

object AsSingleOpenApiParam extends AsOpenParamInstances[AsSingleOpenApiParam]

trait AsOpenParamInstances[TC[x] >: AsSingleOpenApiParam[x]] {
  final implicit def requiredParam[T](implicit
      typ: SwaggerTypeable[T],
      cfg: AsOpenApiParam.Config = AsOpenApiParam.defaultConfig
  ): TC[T]                     =
    AsSingleOpenApiParam[T](typ = typ.typ, cfg)
  final implicit def optParam[T](implicit
      param: AsOpenApiParam[T],
      cfg: AsOpenApiParam.Config = AsOpenApiParam.defaultConfig
  ): AsOpenApiParam[Option[T]] = param.optional
}
