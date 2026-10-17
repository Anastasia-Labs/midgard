import {
  CEK_PROGRAM_MATERIAL_LIMITING_CONSTRAINTS,
  type CekProgramMaterialConcreteTransactionReceipt,
  type CekProgramMaterialLimitingConstraint,
  type CekProgramMaterialLimitingConstraintType,
  type CekProgramMaterialRoute,
  type CekProgramMaterialRouteAttempt,
  confirmationMillisecondsV1,
  exactObject,
  LIMITING_CONSTRAINT_KEYS,
  positiveCount,
  ROUTE_ATTEMPT_KEYS,
  safeSignedInteger,
  signedDecimal,
} from "./validation-dispute-evidence.cek-program-material-necessity-receipt-set.js";
import {
  exactOutRefSequence,
  materialSourceOutRefs,
  measuredConstraintMargin,
  type ParsedTargetProtocolParameters,
  transactionReceipt,
  validateRouteTransactionGrammar,
} from "./validation-dispute-evidence.transaction-receipt.js";

const validateRouteMaterialLinkage = ({
  route,
  transactions,
  minimumMultiOutputCount,
  label,
}: {
  readonly route: CekProgramMaterialRoute;
  readonly transactions: readonly CekProgramMaterialConcreteTransactionReceipt[];
  readonly minimumMultiOutputCount: number | null;
  readonly label: string;
}): void => {
  const publications = transactions.filter(
    (receipt) => receipt.role === "publication",
  );
  for (const publication of publications) {
    if (
      materialSourceOutRefs(publication).length !== 0 ||
      publication.programMaterialOutputOutRefs.length === 0
    ) {
      throw new Error(
        `${label} publications must create material without sourcing prior material`,
      );
    }
  }
  for (const receipt of transactions) {
    if (
      receipt.role !== "publication" &&
      receipt.programMaterialOutputOutRefs.length !== 0
    ) {
      throw new Error(
        `${label} only publication receipts may create program-material outputs`,
      );
    }
  }
  if (route === "directProof") {
    const proof = transactions[0]!;
    if (
      proof.programMaterialInputCount !== 0 ||
      proof.programMaterialReferenceInputCount !== 0 ||
      materialSourceOutRefs(proof).length !== 0 ||
      proof.programMaterialOutputOutRefs.length !== 0
    ) {
      throw new Error(`${label} direct proof must not source program material`);
    }
    return;
  }
  const publicationOutRefs = publications.flatMap(
    (publication) => publication.programMaterialOutputOutRefs,
  );
  if (new Set(publicationOutRefs).size !== publicationOutRefs.length) {
    throw new Error(`${label} contains duplicate published material outrefs`);
  }
  if (route === "completeSinglePublicationReference") {
    const consumption = transactions[1]!;
    if (
      publicationOutRefs.length !== 1 ||
      !exactOutRefSequence(
        materialSourceOutRefs(consumption),
        publicationOutRefs,
      )
    ) {
      throw new Error(
        `${label} single publication and consumption material outrefs do not match`,
      );
    }
    return;
  }
  if (route === "minimumMultiOutputReconstruction") {
    const consumption = transactions.at(-1)!;
    if (
      minimumMultiOutputCount === null ||
      publicationOutRefs.length !== minimumMultiOutputCount ||
      !exactOutRefSequence(
        materialSourceOutRefs(consumption),
        publicationOutRefs,
      )
    ) {
      throw new Error(
        `${label} multi-output publication and reconstruction sources do not match the exact minimum`,
      );
    }
    return;
  }
  const published = new Set(publicationOutRefs);
  const traversed = new Set<string>();
  for (const receipt of transactions.slice(publications.length)) {
    const sources = materialSourceOutRefs(receipt);
    if (
      sources.length === 0 ||
      sources.some((outRef) => !published.has(outRef))
    ) {
      throw new Error(
        `${label} incremental proof receipt contains an empty or unknown material source`,
      );
    }
    for (const source of sources) traversed.add(source);
  }
  if (
    traversed.size !== published.size ||
    publicationOutRefs.some((outRef) => !traversed.has(outRef))
  ) {
    throw new Error(
      `${label} incremental proof receipts omit published material sources`,
    );
  }
};

export const routeAttempt = ({
  value,
  route,
  expectedFit,
  target,
  label,
}: {
  readonly value: unknown;
  readonly route: CekProgramMaterialRoute;
  readonly expectedFit: boolean;
  readonly target: ParsedTargetProtocolParameters;
  readonly label: string;
}): CekProgramMaterialRouteAttempt<
  CekProgramMaterialRoute,
  readonly CekProgramMaterialConcreteTransactionReceipt[],
  number | null
> => {
  const attempt = exactObject(value, ROUTE_ATTEMPT_KEYS, label);
  if (attempt.route !== route || attempt.fit !== expectedFit) {
    throw new Error(
      `${label} must be the ${route} ${expectedFit ? "fit" : "rejected"} attempt`,
    );
  }
  if (
    !Array.isArray(attempt.transactions) ||
    attempt.transactions.length === 0
  ) {
    throw new Error(`${label}.transactions has an invalid receipt count`);
  }
  const transactions = Object.freeze(
    attempt.transactions.map((receipt, index) =>
      transactionReceipt({
        value: receipt,
        target,
        label: `${label}.transactions[${index.toString()}]`,
      }),
    ),
  );
  validateRouteTransactionGrammar({ route, transactions, label });
  const timing = Object.freeze({
    dataAvailabilityFetchMilliseconds: confirmationMillisecondsV1(
      attempt.dataAvailabilityFetchMilliseconds,
      `${label}.dataAvailabilityFetchMilliseconds`,
    ),
    evidenceConstructionMilliseconds: confirmationMillisecondsV1(
      attempt.evidenceConstructionMilliseconds,
      `${label}.evidenceConstructionMilliseconds`,
    ),
    retryMilliseconds: confirmationMillisecondsV1(
      attempt.retryMilliseconds,
      `${label}.retryMilliseconds`,
    ),
    rollbackAllowanceMilliseconds: confirmationMillisecondsV1(
      attempt.rollbackAllowanceMilliseconds,
      `${label}.rollbackAllowanceMilliseconds`,
    ),
    settlementMilliseconds: confirmationMillisecondsV1(
      attempt.settlementMilliseconds,
      `${label}.settlementMilliseconds`,
    ),
    removalMilliseconds: confirmationMillisecondsV1(
      attempt.removalMilliseconds,
      `${label}.removalMilliseconds`,
    ),
  });
  const maturityWindowMarginMilliseconds = safeSignedInteger(
    attempt.maturityWindowMarginMilliseconds,
    `${label}.maturityWindowMarginMilliseconds`,
  );
  const totalConfirmationMilliseconds = transactions.reduce(
    (total, receipt) => total + receipt.confirmationMilliseconds,
    0,
  );
  const totalCorrectionPathMilliseconds = Object.values(timing).reduce(
    (total, component) => total + component,
    totalConfirmationMilliseconds,
  );
  if (
    !Number.isSafeInteger(totalCorrectionPathMilliseconds) ||
    maturityWindowMarginMilliseconds !==
      Math.floor(target.maturityWindowMilliseconds / 2) -
        totalCorrectionPathMilliseconds
  ) {
    throw new Error(`${label} contains an invalid maturity-window margin`);
  }
  let minimumMultiOutputCount: number | null;
  if (route === "minimumMultiOutputReconstruction") {
    minimumMultiOutputCount = positiveCount(
      attempt.minimumMultiOutputCount,
      `${label}.minimumMultiOutputCount`,
    );
    if (minimumMultiOutputCount < 2) {
      throw new Error(`${label}.minimumMultiOutputCount must be at least two`);
    }
  } else if (attempt.minimumMultiOutputCount !== null) {
    throw new Error(
      `${label}.minimumMultiOutputCount is invalid for the selected route`,
    );
  } else {
    minimumMultiOutputCount = null;
  }
  validateRouteMaterialLinkage({
    route,
    transactions,
    minimumMultiOutputCount,
    label,
  });
  let limitingConstraint: CekProgramMaterialLimitingConstraint | null = null;
  if (expectedFit) {
    if (
      attempt.limitingConstraint !== null ||
      maturityWindowMarginMilliseconds < 0 ||
      transactions.some(
        (receipt) =>
          receipt.transactionByteMargin < 0 ||
          receipt.maximumValueByteMargin < 0 ||
          BigInt(receipt.executionMemoryMargin) < 0n ||
          BigInt(receipt.executionCpuMargin) < 0n,
      )
    ) {
      throw new Error(`${label} fit attempt contains a failed constraint`);
    }
  } else {
    const constraint = exactObject(
      attempt.limitingConstraint,
      LIMITING_CONSTRAINT_KEYS,
      `${label}.limitingConstraint`,
    );
    if (
      typeof constraint.type !== "string" ||
      !CEK_PROGRAM_MATERIAL_LIMITING_CONSTRAINTS.includes(
        constraint.type as CekProgramMaterialLimitingConstraintType,
      )
    ) {
      throw new Error(`${label}.limitingConstraint.type is invalid`);
    }
    const measuredMargin = signedDecimal(
      constraint.measuredMargin,
      `${label}.limitingConstraint.measuredMargin`,
    );
    if (
      measuredMargin !==
        measuredConstraintMargin({
          constraint:
            constraint.type as CekProgramMaterialLimitingConstraintType,
          transactions,
          maturityWindowMarginMilliseconds,
        }) ||
      BigInt(measuredMargin) >= 0n
    ) {
      throw new Error(`${label} contains an invalid limiting measured margin`);
    }
    limitingConstraint = Object.freeze({
      type: constraint.type as CekProgramMaterialLimitingConstraintType,
      measuredMargin,
    });
  }
  return Object.freeze({
    route,
    transactions,
    ...timing,
    maturityWindowMarginMilliseconds,
    fit: expectedFit,
    limitingConstraint,
    minimumMultiOutputCount,
  });
};
