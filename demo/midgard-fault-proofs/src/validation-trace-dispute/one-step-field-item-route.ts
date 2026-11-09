/** How a step's resolution reads one item of a field of the disputed
 * transaction: by walking the field, or through the field's grammar
 * certificate, which reads the item without walking the field, so the
 * step's cost no longer grows with the field's item count. A stage that
 * keeps both routes takes the route beside its committed evidence, never in
 * it: in its own redeemer, or in the withdraw redeemer of the item yield
 * that opens the item. The paths that read only through the certificate
 * take its reference-input index alone, or the certified route. The rule
 * choosing the route is `validationTraceFieldItemRoute`, here alone. */
import {
  type MidgardFieldCarriagePlan,
  planMidgardFieldCarriage,
} from "@al-ft/midgard-core";
import {
  decodeMidgardFieldPreimage,
  MIDGARD_WALK_DERIVED_STRIDE,
  midgardFieldStride,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type RedeemerContext,
  type UTxO,
} from "@lucid-evolution/lucid";

import { findCompleteFaultProofFieldGrammarCertificate } from "../field-grammar-certificate.js";
import type { ValidationTraceClaimMaterial } from "./claim-material.js";
import { constr, integer } from "./one-step-data.js";
import type { ValidationTracePhaseYield } from "./one-step-phase-yields.js";
import type { ValidationTraceOneStepPlan } from "./one-step-plan.js";
import type { ValidationTraceDisputeDeployment } from "./transactions.js";

/**
 * The most items of a variable-width field a stage that keeps both routes
 * walks. The walking routes cost about 0.1M memory units an item (the
 * walking rows of `certified_field_item_v1` at the field's maximum), so the
 * fewest items a dual-route stage walks inside the per-step budget is about
 * 147 (the stage-four begin over field-2 outputs); past this bound, with
 * room to spare for wider items, the stage reads through the certificate.
 */
export const VALIDATION_TRACE_WALK_ROUTE_MAX_ITEMS = 100;

/** Where a stage takes its route: in its own redeemer beside the evidence
 * (`MachineFieldItemRouteV1`), as the certificate's bare reference-input
 * index (a stage that reads only through the certificate), or in the
 * withdraw redeemer of the item yield it invokes. */
export type ValidationTraceFieldItemRouteCarrier =
  | "redeemer"
  | "certificateIndex"
  | "yield";

/** The item read a step's resolution makes through a door that takes a
 * route: the field it reads, at which holder's move, where that move
 * carries the route, and whether the stage reads only through the
 * certificate. */
export type ValidationTraceFieldItemRead = Readonly<{
  holder: string;
  fieldIndex: number;
  carrier: ValidationTraceFieldItemRouteCarrier;
  certifiedOnly: boolean;
}>;

/** The item yields that take a route, with the field each opens and
 * whether it reads only through the certificate. */
const ROUTED_ITEM_YIELDS: Readonly<
  Partial<
    Record<
      ValidationTracePhaseYield,
      readonly [fieldIndex: number, certifiedOnly: boolean]
    >
  >
> = Object.freeze({
  phaseANativeItemNative: [6, false],
  phaseANativeItemForeign: [6, true],
  scriptSourcesStageFourBegin: [2, false],
  scriptSourcesStageSixBeginPolicy: [5, true],
});

/** The stage-zero begin semantic resolver, which takes the route in its
 * redeemer, and the stage-one redeemer semantic resolver, which takes the
 * certificate's index alone (field 8). */
const STAGE_ZERO_BEGIN_SEMANTIC = "scriptSourcesStageZeroBeginSemantic";
const STAGE_ONE_REDEEMER_SEMANTIC = "scriptSourcesStageOneRedeemerSemantic";
const REDEEMERS_FIELD = 8;

/** The item read of `plan`'s resolution through a routed door, if any. */
export const validationTraceFieldItemRead = (
  plan: ValidationTraceOneStepPlan,
): ValidationTraceFieldItemRead | undefined => {
  if (plan.kind === "canonicalDecode") {
    const item = plan.canonicalDecode.item;
    return item === undefined
      ? undefined
      : Object.freeze({
          holder: "canonicalDecodeItemObserve",
          fieldIndex: item.fieldIndex,
          carrier: "redeemer" as const,
          certifiedOnly: false,
        });
  }
  if (plan.kind !== "phase") return undefined;
  const { semantic, evidenceCbors, itemYield } = plan.phase;
  if (semantic === STAGE_ZERO_BEGIN_SEMANTIC) {
    const fieldIndex = Data.from(evidenceCbors[0] ?? "");
    if (typeof fieldIndex !== "bigint")
      throw new Error(
        "validation-trace stage-zero begin evidence names no field",
      );
    return Object.freeze({
      holder: semantic,
      fieldIndex: Number(fieldIndex),
      carrier: "redeemer" as const,
      certifiedOnly: false,
    });
  }
  if (semantic === STAGE_ONE_REDEEMER_SEMANTIC)
    return Object.freeze({
      holder: semantic,
      fieldIndex: REDEEMERS_FIELD,
      carrier: "certificateIndex" as const,
      certifiedOnly: true,
    });
  const routed =
    itemYield === undefined ? undefined : ROUTED_ITEM_YIELDS[itemYield.name];
  if (routed === undefined) return undefined;
  const [fieldIndex, certifiedOnly] = routed;
  return Object.freeze({
    holder: semantic,
    fieldIndex,
    carrier: "yield" as const,
    certifiedOnly,
  });
};

/**
 * The route the read `read` of the field whose preimage is `preimage`
 * takes: through the certificate on a stage that reads only through it, by
 * walking a fixed-stride field (which has no certificate), and otherwise by
 * walking a field of at most `VALIDATION_TRACE_WALK_ROUTE_MAX_ITEMS` items.
 */
export const validationTraceFieldItemRoute = (
  read: Pick<ValidationTraceFieldItemRead, "fieldIndex" | "certifiedOnly">,
  preimage: Uint8Array,
): "walk" | "certified" => {
  if (read.certifiedOnly) return "certified";
  if (midgardFieldStride(read.fieldIndex) !== MIDGARD_WALK_DERIVED_STRIDE)
    return "walk";
  return decodeMidgardFieldPreimage(Buffer.from(preimage)).length >
    VALIDATION_TRACE_WALK_ROUTE_MAX_ITEMS
    ? "certified"
    : "walk";
};

/** The most bytes the certified route adds to a move over the walking
 * route: the route's certificate index (up to 3 bytes over
 * `WalkFieldItem`) and the certificate's reference input (a 32-byte id and
 * an index under 256: 37 bytes, and up to 5 more for a reference-input set
 * the move did not have). */
export const VALIDATION_TRACE_CERTIFIED_ROUTE_MAX_EXTRA_BYTES = 45;

/** The room the route of a canonical-decode observe of an item of field
 * `fieldIndex`, whose preimage is `preimage`, takes from the observe move's
 * inline carriage: none walking, the certificate's read through it. */
export const validationTraceObserveRouteExtraBytes = (
  fieldIndex: number,
  preimage: Uint8Array,
): number =>
  validationTraceFieldItemRoute(
    { fieldIndex, certifiedOnly: false },
    preimage,
  ) === "certified"
    ? VALIDATION_TRACE_CERTIFIED_ROUTE_MAX_EXTRA_BYTES
    : 0;

/** The carriage plan whose field a certificate of the disputed
 * transaction's field `fieldIndex` in `material` names. */
export const validationTraceFieldGrammarPlan = (
  material: ValidationTraceClaimMaterial,
  fieldIndex: number,
  owner: string,
): MidgardFieldCarriagePlan =>
  planMidgardFieldCarriage({
    owner: Buffer.from(owner, "hex"),
    txId: Buffer.from(material.fieldSource().nativeTxId, "hex"),
    fieldIndex,
    preimage: material.fieldPreimage(fieldIndex),
  });

/** A routed item read as a move makes it: the read, and the `Complete`
 * certificate it reads through, none on the walking route. */
export type ValidationTraceFieldItemRouteReads = Readonly<{
  read: ValidationTraceFieldItemRead;
  certificate: UTxO | undefined;
}>;

const certificateIndex = (context: RedeemerContext, certificate: UTxO) =>
  SDK.requireReferenceInputIndex(
    context,
    certificate,
    "field grammar certificate",
  );

/** The route `reads` takes as its `MachineFieldItemRouteV1`, in the final
 * transaction `context`: `WalkFieldItem`, or `CertifiedFieldItem` at the
 * certificate's reference-input index. */
export const fieldItemRouteCbor = (
  reads: ValidationTraceFieldItemRouteReads,
  context: RedeemerContext,
): string =>
  reads.certificate === undefined
    ? constr(0, [])
    : constr(1, [integer(certificateIndex(context, reads.certificate))]);

/** The certificate's reference-input index, as a stage that reads only
 * through the certificate takes it. */
export const fieldItemCertificateIndexCbor = (
  reads: ValidationTraceFieldItemRouteReads,
  context: RedeemerContext,
): string => {
  if (reads.certificate === undefined)
    throw new Error(
      `validation-trace ${reads.read.holder} reads field ${reads.read.fieldIndex.toString()} only through its field grammar certificate`,
    );
  return integer(certificateIndex(context, reads.certificate));
};

/** The certificate a move reads beside its other reference inputs. */
export const fieldItemRouteReferences = (
  reads: ValidationTraceFieldItemRouteReads | undefined,
): readonly UTxO[] =>
  reads?.certificate === undefined ? [] : [reads.certificate];

/** Our `Complete` certificate (datum `owner` is `owner`) of the disputed
 * transaction's field `fieldIndex` under `deployment`'s certificate policy;
 * none while there is none. The step relies only on a certificate the
 * prover controls. */
export const findValidationTraceFieldGrammarCertificate = async ({
  lucid,
  deployment,
  material,
  fieldIndex,
  owner,
}: {
  readonly lucid: LucidEvolution;
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly material: ValidationTraceClaimMaterial;
  readonly fieldIndex: number;
  readonly owner: string;
}): Promise<UTxO | undefined> => {
  const certificate = deployment.fieldGrammarCertificate;
  if (certificate === undefined)
    throw new Error(
      "validation-trace deployment has no field grammar certificate",
    );
  return await findCompleteFaultProofFieldGrammarCertificate({
    lucid,
    network: deployment.network,
    certificatePolicyId: certificate.validator.spendingScriptHash,
    plan: validationTraceFieldGrammarPlan(material, fieldIndex, owner),
    ownerKeyHash: owner,
  });
};

/**
 * What the move of a thread at `holder` in the resolution `plan` reads for
 * its routed item read: at the read's own holder, and, for a phase step
 * whose evidence names our publications of its field by their positions
 * among the reader's reference inputs, at every move past the boundary,
 * whose evidence those positions bind. None where the step makes no routed
 * read. Throws where the read goes through a certificate not yet
 * `Complete`.
 */
export const validationTraceFieldItemRouteAt = async ({
  lucid,
  deployment,
  material,
  plan,
  holder,
  owner,
}: {
  readonly lucid: LucidEvolution;
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly material: ValidationTraceClaimMaterial | undefined;
  readonly plan: ValidationTraceOneStepPlan;
  readonly holder: string;
  readonly owner: string;
}): Promise<ValidationTraceFieldItemRouteReads | undefined> => {
  const read = validationTraceFieldItemRead(plan);
  if (read === undefined || holder === "boundary") return undefined;
  if (
    holder !== read.holder &&
    !(plan.kind === "phase" && plan.carried !== undefined)
  )
    return undefined;
  if (material === undefined)
    throw new Error(
      "validation-trace one step has no claim material to read its field from",
    );
  if (
    validationTraceFieldItemRoute(
      read,
      material.fieldPreimage(read.fieldIndex),
    ) === "walk"
  )
    return Object.freeze({ read, certificate: undefined });
  const certificate = await findValidationTraceFieldGrammarCertificate({
    lucid,
    deployment,
    material,
    fieldIndex: read.fieldIndex,
    owner,
  });
  if (certificate === undefined)
    throw new Error(
      `validation-trace ${read.holder} reads field ${read.fieldIndex.toString()} through its field grammar certificate, which is not yet Complete`,
    );
  return Object.freeze({ read, certificate });
};
