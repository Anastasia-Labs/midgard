import { createHash } from "node:crypto";

import {
  completeRootTuple,
  isNonEmptyString,
  jsonEqual,
  validateArchitectureGPhase1FormalBindingIdentity,
  validateArchitectureGRuntimeIdentity,
} from "./mpf-architecture-g-gate-config.capture-architecture-gphase1-formal-binding-identity.mjs";
import { validateArchitectureGCanonicalCorpusIdentity } from "./mpf-architecture-g-gate-config.validate-architecture-gcanonical-corpus-identity.mjs";
import {
  ARCHITECTURE_G_FORMAL_GATE_CONFIG,
  isCanonicalAbsolutePath,
  isCanonicalTimestamp,
  isHash,
  isNonNegativeFiniteNumber,
  isPositiveSafeInteger,
  requireExactObjectKeys,
} from "./mpf-architecture-g-gate-config.validate-architecture-gfixture-creation-evidence.mjs";
import {
  percentile,
  validateArchitectureGRootGateResultShape,
} from "./mpf-architecture-g-gate-config.validate-architecture-groot-gate-result-shape.mjs";

export const validateArchitectureGRootGateSummary = ({
  summary,
  mode,
  runs,
  transactions,
  cpuSet,
}) => {
  requireExactObjectKeys(
    summary,
    [
      "schemaVersion",
      "formal",
      "profile",
      "requiredCardinality",
      "generatedAt",
      "mode",
      "freshProcessRunsPerFixture",
      "transactionCount",
      "phase1FormalBinding",
      "runtimeIdentity",
      "canonicalCorpus",
      "binaryPath",
      "binarySha256",
      "probePath",
      "probeSha256",
      "gitHead",
      "sourceSha256",
      "diffSha256",
      "gitStatusSha256",
      "gitStatusEntries",
      "sourceFiles",
      "cpuSet",
      "nodeOptions",
      "cgroup",
      "percentileMethod",
      "groups",
      "verdict",
    ],
    "Architecture G production root-gate summary",
  );
  requireExactObjectKeys(
    summary.requiredCardinality,
    ["runs", "transactions"],
    "Architecture G required cardinality",
  );
  validateArchitectureGPhase1FormalBindingIdentity(
    summary?.phase1FormalBinding,
  );
  validateArchitectureGRuntimeIdentity({
    identity: summary?.runtimeIdentity,
    expectedVersion: summary?.runtimeIdentity?.version,
    expectedExecutableSha256: summary?.runtimeIdentity?.executableSha256,
  });
  const formal = summary.formal === true;
  if (
    summary?.schemaVersion !==
      (formal
        ? "midgard-architecture-g-production-root-gate-v1"
        : "midgard-architecture-g-root-diagnostic-smoke-v1") ||
    (summary.formal !== true && summary.formal !== false) ||
    summary.profile !== (formal ? "formal" : "smoke") ||
    summary.mode !== mode ||
    !jsonEqual(
      summary.requiredCardinality,
      ARCHITECTURE_G_FORMAL_GATE_CONFIG[mode],
    ) ||
    summary.freshProcessRunsPerFixture !== runs ||
    summary.transactionCount !== transactions ||
    summary.cpuSet !== cpuSet ||
    !isCanonicalTimestamp(summary.generatedAt)
  ) {
    throw new Error("Architecture G root gate summary identity is invalid");
  }
  if (
    !isCanonicalAbsolutePath(summary.probePath) ||
    !isHash(summary.probeSha256) ||
    !isCanonicalAbsolutePath(summary.binaryPath) ||
    !isHash(summary.binarySha256)
  ) {
    throw new Error("Architecture G root gate executable identity is invalid");
  }
  requireExactObjectKeys(
    summary.cgroup,
    ["membership", "memoryMaxPath", "memoryMax"],
    "Architecture G root gate cgroup identity",
  );
  const statusEntries = summary.gitStatusEntries;
  const sourceFiles = summary.sourceFiles;
  const canonicalStatusBytes = Buffer.from(
    Array.isArray(statusEntries) && statusEntries.length > 0
      ? `${statusEntries.join("\0")}\0`
      : "",
  );
  if (
    !/^(?:[0-9a-f]{40}|[0-9a-f]{64})$/u.test(summary.gitHead) ||
    !isHash(summary.sourceSha256) ||
    !isHash(summary.diffSha256) ||
    !isHash(summary.gitStatusSha256) ||
    !Array.isArray(statusEntries) ||
    !statusEntries.every(
      (entry) =>
        typeof entry === "string" &&
        entry.length > 0 &&
        entry.length <= 4096 &&
        !entry.includes("\0"),
    ) ||
    createHash("sha256").update(canonicalStatusBytes).digest("hex") !==
      summary.gitStatusSha256 ||
    !Array.isArray(sourceFiles) ||
    sourceFiles.length === 0 ||
    !sourceFiles.every(
      (path) =>
        typeof path === "string" &&
        path.length > 0 &&
        path.length <= 4096 &&
        !path.includes("\0"),
    ) ||
    new Set(sourceFiles).size !== sourceFiles.length ||
    !jsonEqual(sourceFiles, [...sourceFiles].sort()) ||
    summary.nodeOptions !== "--max-old-space-size=4096" ||
    !isNonEmptyString(summary.cgroup.membership) ||
    !isNonEmptyString(summary.cgroup.memoryMaxPath) ||
    !isNonEmptyString(summary.cgroup.memoryMax) ||
    summary.percentileMethod !==
      "nearest-rank: sorted[max(0, ceil(N*q)-1)]; q=0.5 median, q=0.95 p95"
  ) {
    throw new Error("Architecture G root gate provenance is invalid");
  }
  const canonicalEvidence =
    summary.canonicalCorpus === null
      ? null
      : validateArchitectureGCanonicalCorpusIdentity({
          canonicalCorpus: summary.canonicalCorpus,
          phase1FormalBinding: summary.phase1FormalBinding,
          transactions,
        });
  if (formal && canonicalEvidence === null) {
    throw new Error(
      "Architecture G formal root gate requires canonical corpus evidence",
    );
  }
  const expectedCanonicalSlice = canonicalEvidence?.canonicalSlice ?? null;
  const expectedCanonicalFunding = canonicalEvidence?.canonicalFunding ?? null;
  const expectedSizes =
    mode === "50k" ? [1_000_000] : [100_000, 300_000, 1_000_000];
  if (
    !Array.isArray(summary.groups) ||
    summary.groups.length !== expectedSizes.length ||
    !jsonEqual(
      summary.groups.map((group) => group?.initialUtxos),
      expectedSizes,
    )
  ) {
    throw new Error("Architecture G root gate fixture groups are incomplete");
  }
  for (const group of summary.groups) {
    requireExactObjectKeys(
      group,
      [
        "initialUtxos",
        "fixtureCreation",
        "fixtureBefore",
        "fixtureAfter",
        "roots",
        "durationMs",
        "results",
      ],
      "Architecture G root-gate fixture group",
    );
    const fixture = group.fixtureBefore;
    const after = group.fixtureAfter;
    const creation = group.fixtureCreation;
    if (formal) {
      requireExactObjectKeys(
        creation,
        [
          "path",
          "sha256",
          "initialUtxoCount",
          "marker",
          "utxoPayloadAggregate",
        ],
        "Architecture G fixture-creation identity",
      );
      requireExactObjectKeys(
        creation.utxoPayloadAggregate,
        ["entryCount", "encodedTupleBytes"],
        "Architecture G fixture payload aggregate",
      );
    } else if (creation !== null) {
      throw new Error(
        "Architecture G smoke root gate cannot claim formal fixture-creation evidence",
      );
    }
    for (const [value, label] of [
      [fixture, "Architecture G fixture-before identity"],
      [after, "Architecture G fixture-after identity"],
    ]) {
      requireExactObjectKeys(
        value,
        ["path", "directoryBytes", "logicalSha256", "records", "marker"],
        label,
      );
    }
    if (
      (formal &&
        (creation?.initialUtxoCount !== group.initialUtxos ||
          creation?.marker !== fixture?.marker ||
          creation?.utxoPayloadAggregate?.entryCount !== group.initialUtxos ||
          !Number.isSafeInteger(
            creation?.utxoPayloadAggregate?.encodedTupleBytes,
          ) ||
          creation.utxoPayloadAggregate.encodedTupleBytes <= 0 ||
          !isCanonicalAbsolutePath(creation?.path) ||
          !isHash(creation?.sha256))) ||
      !isCanonicalAbsolutePath(fixture?.path) ||
      !isPositiveSafeInteger(fixture?.directoryBytes) ||
      !isHash(fixture?.marker) ||
      !isHash(fixture?.logicalSha256) ||
      fixture?.records !== group.initialUtxos + 1 ||
      after?.path !== fixture.path ||
      after?.directoryBytes !== fixture.directoryBytes ||
      fixture?.marker !== after?.marker ||
      fixture?.logicalSha256 !== after?.logicalSha256 ||
      fixture?.records !== after?.records
    ) {
      throw new Error(
        `Architecture G root gate fixture evidence is invalid at ${String(group.initialUtxos)}`,
      );
    }
    if (!Array.isArray(group.results) || group.results.length !== runs) {
      throw new Error(
        `Architecture G root gate run count is invalid at ${String(group.initialUtxos)}`,
      );
    }
    const expectedRoots = group.roots;
    requireExactObjectKeys(
      expectedRoots,
      [
        "utxoRoot",
        "rawTxRoot",
        "txRoot",
        "transitionTraceRoot",
        "eventToStepRoot",
        "depositsRoot",
        "withdrawalsRoot",
        "forcedTransactionsRoot",
        "transitionRoots",
      ],
      "Architecture G root-gate complete roots",
    );
    requireExactObjectKeys(
      group.durationMs,
      ["min", "median", "p95", "max"],
      "Architecture G root-gate duration aggregate",
    );
    if (
      ![
        expectedRoots?.utxoRoot,
        expectedRoots?.rawTxRoot,
        expectedRoots?.txRoot,
        expectedRoots?.transitionTraceRoot,
        expectedRoots?.eventToStepRoot,
        expectedRoots?.depositsRoot,
        expectedRoots?.withdrawalsRoot,
        expectedRoots?.forcedTransactionsRoot,
      ].every(isHash) ||
      !Array.isArray(expectedRoots?.transitionRoots) ||
      expectedRoots.transitionRoots.length !== transactions
    ) {
      throw new Error(
        `Architecture G root gate complete roots are invalid at ${String(group.initialUtxos)}`,
      );
    }
    for (const result of group.results) {
      validateArchitectureGRootGateResultShape(result);
      if (
        result?.engine !== "architecture_g" ||
        result?.probePath !== summary.probePath ||
        result?.probeSha256 !== summary.probeSha256 ||
        result?.binarySha256 !== summary.binarySha256 ||
        !jsonEqual(result?.canonicalCorpusSlice, expectedCanonicalSlice) ||
        !jsonEqual(result?.canonicalFunding, expectedCanonicalFunding) ||
        result?.cpuAffinity !== cpuSet ||
        result?.transactionCount !== transactions ||
        result?.initialUtxoCount !== group.initialUtxos ||
        result?.levelBackedInitialView !== true ||
        result?.reusedLevelFixture !== true ||
        !isPositiveSafeInteger(result?.ledgerOpCount) ||
        !isNonNegativeFiniteNumber(result?.startupMs) ||
        result?.confirmedLedgerFullScans !== 0 ||
        !Number.isFinite(result?.durationMs) ||
        result.durationMs <= 0 ||
        result.buildPlusCaptureMs !== result.durationMs ||
        !Object.values(result.phaseMs).every(isNonNegativeFiniteNumber) ||
        !Object.values(result.nativePhaseMs).every(isNonNegativeFiniteNumber) ||
        !Object.values(result.pathHydration).every(isNonNegativeFiniteNumber) ||
        !isHash(result.workloadSha256) ||
        !Array.isArray(result.transitionRoots) ||
        result.transitionRoots.length !== transactions ||
        !result.transitionRoots.every(
          (transition) => isHash(transition?.pre) && isHash(transition?.post),
        ) ||
        !isHash(result.ownerBefore?.durableRoot) ||
        result.ownerBefore.durableRoot !== fixture.marker ||
        result.ownerAfter?.durableRoot !== fixture.marker ||
        !jsonEqual(
          result.ownerBefore?.ownerEpoch,
          result.ownerAfter?.ownerEpoch,
        ) ||
        result.ownerBefore?.childRestarts !==
          result.ownerAfter?.childRestarts ||
        result.transitionRoots[0]?.pre !== result.ownerBefore?.durableRoot ||
        result.transitionRoots.at(-1)?.post !== result.utxoRoot ||
        !jsonEqual(completeRootTuple(result), expectedRoots)
      ) {
        throw new Error(
          `Architecture G root gate result evidence is invalid at ${String(group.initialUtxos)}`,
        );
      }
      for (let index = 1; index < result.transitionRoots.length; index += 1) {
        if (
          result.transitionRoots[index]?.pre !==
          result.transitionRoots[index - 1]?.post
        ) {
          throw new Error(
            `Architecture G root gate transition chain is invalid at ${String(group.initialUtxos)}`,
          );
        }
      }
    }
    const durations = group.results.map((result) => result.durationMs);
    const expectedDuration = {
      min: Math.min(...durations),
      median: percentile(durations, 0.5),
      p95: percentile(durations, 0.95),
      max: Math.max(...durations),
    };
    if (!jsonEqual(group.durationMs, expectedDuration)) {
      throw new Error(
        `Architecture G root gate duration evidence is invalid at ${String(group.initialUtxos)}`,
      );
    }
  }
  if (
    mode === "growth" &&
    (summary.groups.some((group) =>
      group.results.some((result) => !isHash(result?.workloadSha256)),
    ) ||
      new Set(
        summary.groups.flatMap((group) =>
          group.results.map((result) => result.workloadSha256),
        ),
      ).size !== 1)
  ) {
    throw new Error("Architecture G growth workload identity is invalid");
  }
  const expectedVerdict =
    mode === "50k"
      ? {
          pass: summary.groups[0].durationMs.p95 < 10_000,
          gate: "50k_complete_root_build_p95_under_10s",
          p95Ms: summary.groups[0].durationMs.p95,
          limitMs: 10_000,
        }
      : (() => {
          const medians = summary.groups.map(
            (group) => group.durationMs.median,
          );
          const minimumMedianMs = Math.min(...medians);
          const maximumMedianMs = Math.max(...medians);
          const maxMinSlopePercent =
            ((maximumMedianMs - minimumMedianMs) / minimumMedianMs) * 100;
          return {
            pass: maxMinSlopePercent <= 10,
            gate: "100k_300k_1m_max_min_build_slope_within_10_percent",
            maxMinSlopePercent,
            minimumMedianMs,
            maximumMedianMs,
            limitAbsolutePercent: 10,
          };
        })();
  requireExactObjectKeys(
    summary.verdict,
    mode === "50k"
      ? ["pass", "gate", "p95Ms", "limitMs"]
      : [
          "pass",
          "gate",
          "maxMinSlopePercent",
          "minimumMedianMs",
          "maximumMedianMs",
          "limitAbsolutePercent",
        ],
    "Architecture G root-gate verdict",
  );
  if (!expectedVerdict.pass || !jsonEqual(summary.verdict, expectedVerdict)) {
    throw new Error("Architecture G root gate verdict is invalid or failed");
  }
  return summary;
};
