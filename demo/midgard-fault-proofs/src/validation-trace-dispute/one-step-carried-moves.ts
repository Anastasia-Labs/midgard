/** The moves of a thread of ours whose step's evidence reads a field by
 * reference (one-step-carriage.ts). The evidence names our publications of
 * the field by their positions among the reference inputs of the move that
 * opens it (the reader), and the move that commits the evidence commits
 * those positions; so every move from that one up to the award binds the
 * evidence to our live publications, at the positions they hold among the
 * reader's reference inputs. The reader, and for a CEK context step the
 * observer fold and the settle past it, read the publications as reference
 * inputs, which keeps them ours to spend back only once every thread of
 * the case reading them has left them behind (carried-field.ts). */
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { Data, type UTxO } from "@lucid-evolution/lucid";

import type { ValidationTraceHop } from "./one-step-builders.js";
import {
  carriageAmong,
  carriageCbor,
  carriageReads,
  type ValidationTraceCarriedField,
  type ValidationTraceFieldCarriage,
} from "./one-step-carriage.js";
import {
  type ValidationTraceOneStepContract,
  validationTraceOneStepHolder,
} from "./one-step-holders.js";
import { phaseYieldListReads, phaseYieldReads } from "./one-step-phase.js";
import type { ValidationTraceOneStepPlan } from "./one-step-plan.js";
import type { ValidationTraceDisputeDeployment } from "./transactions.js";

type Holder = ValidationTraceOneStepContract["holder"];

/** The field the move of a thread at `holder` in the resolution `plan`
 * reads by reference, or binds its evidence to our publications of: a
 * canonical-decode item's at its observe move, which commits none of it,
 * and any other step's at each move past the boundary. */
export const validationTraceCarriedFieldAt = (
  plan: ValidationTraceOneStepPlan,
  holder: Holder,
): ValidationTraceCarriedField | undefined =>
  plan.kind === "canonicalDecode"
    ? holder === "canonicalDecodeItemObserve"
      ? plan.canonicalDecode.item?.carried
      : undefined
    : holder === "boundary"
      ? undefined
      : plan.carried?.field;

/** Whether the move of a thread at `holder` in `plan` reads our
 * publications of its field as reference inputs. */
const readsCarriage = (plan: ValidationTraceOneStepPlan, holder: Holder) =>
  plan.carried !== undefined &&
  (holder === plan.carried.reader ||
    (plan.kind === "context" &&
      (holder === "contextObserverFold" || holder === "contextSettle")));

/** The reference inputs the reader of the field `plan` carries reads, with
 * our publications of it `carriage`: its holder's script, the yields a
 * phase semantic resolver invokes, and the publications. */
const readerReferences = (
  deployment: ValidationTraceDisputeDeployment,
  plan: ValidationTraceOneStepPlan,
  reader: Holder,
  carriage: ValidationTraceFieldCarriage,
): readonly UTxO[] => {
  const { reference } = validationTraceOneStepHolder(deployment, reader);
  if (reference === undefined)
    throw new Error(`validation-trace deployment publishes no ${reader}`);
  const item =
    plan.kind === "phase" ? phaseYieldReads(deployment, plan.phase) : undefined;
  const list =
    plan.kind === "phase"
      ? phaseYieldListReads(deployment, plan.phase)
      : undefined;
  const yields = [
    ...(item === undefined ? [] : [item.reference]),
    ...(list?.references ?? []),
  ];
  return [reference, ...yields, ...carriageReads(carriage)];
};

/** Where a holder whose thread the evidence-committing move left keeps the
 * evidence it committed, for the holders a field-reading step reaches next:
 * a phase semantic resolver and the CEK context step resolver. */
const COMMITTED_EVIDENCE = [1, 0, 2] as const;

/**
 * The resolution `plan` of a thread of ours at `holder`, whose datum is
 * `datum`, bound to our publications `carriage` of the field it reads, and
 * the hop `hop` builds of it, reading them where the move does; throws
 * where the thread's holder committed other evidence, and, at the reader,
 * should the move's reference inputs place them elsewhere.
 */
export const validationTraceCarriedMove = ({
  deployment,
  plan,
  holder,
  datum,
  carriage,
  hop,
}: {
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly plan: ValidationTraceOneStepPlan;
  readonly holder: Holder;
  readonly datum: string;
  readonly carriage: ValidationTraceFieldCarriage | undefined;
  readonly hop: (plan: ValidationTraceOneStepPlan) => ValidationTraceHop;
}): ValidationTraceHop => {
  const { carried } = plan;
  if (carried === undefined || carriage === undefined) return hop(plan);
  const bound = carriageAmong(
    carriage,
    readerReferences(deployment, plan, carried.reader, carriage),
  );
  const rebound = carried.rebind(bound);
  if (
    (holder === "context" ||
      (plan.kind === "phase" && holder === plan.phase.semantic)) &&
    Data.from<string>(plutusConstrFieldCbor(datum, [...COMMITTED_EVIDENCE])) !==
      rebound.evidenceHash
  )
    throw new Error(
      `validation-trace ${holder} holds evidence other than our publications of its field now name`,
    );
  const built = hop(rebound);
  if (!readsCarriage(plan, holder)) return built;
  const expected = carriageCbor(bound);
  return Object.freeze({
    ...built,
    redeemer: (inputIndex, outputIndex, context) => {
      if (
        holder === carried.reader &&
        carriageCbor(carriageAmong(carriage, context.referenceInputs)) !==
          expected
      )
        throw new Error(
          `validation-trace ${holder} move reads our publications of its field at other positions than its evidence names`,
        );
      return built.redeemer(inputIndex, outputIndex, context);
    },
    reads: {
      references: [
        ...(built.reads?.references ?? []),
        ...carriageReads(carriage),
      ],
      yields: built.reads?.yields ?? [],
    },
  });
};
