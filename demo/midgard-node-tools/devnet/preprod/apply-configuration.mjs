import { readFile, writeFile } from "node:fs/promises";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

/** Apply current ledger parameters at genesis; retain this isolated chain's keys. */
export function applyPreprodConfiguration(genesis, profile) {
  const p = profile.protocolParameters;
  const shelley = {
    ...genesis.shelley,
    ...profile.consensus,
    protocolParams: {
      ...genesis.shelley.protocolParams,
      protocolVersion: p.protocolVersion,
      minFeeA: p.txFeePerByte,
      minFeeB: p.txFeeFixed,
      maxBlockBodySize: p.maxBlockBodySize,
      maxBlockHeaderSize: p.maxBlockHeaderSize,
      maxTxSize: p.maxTxSize,
      keyDeposit: p.stakeAddressDeposit,
      poolDeposit: p.stakePoolDeposit,
      eMax: p.poolRetireMaxEpoch,
      nOpt: p.stakePoolTargetNum,
      a0: p.poolPledgeInfluence,
      rho: p.monetaryExpansion,
      tau: p.treasuryCut,
      minPoolCost: p.minPoolCost,
    },
    extraConfig: {
      ...genesis.shelley.extraConfig,
      stakePools: {
        data: Object.fromEntries(
          Object.entries(genesis.shelley.extraConfig.stakePools.data).map(
            ([id, pool]) => [id, { ...pool, cost: p.minPoolCost }],
          ),
        ),
      },
    },
  };
  const alonzo = {
    ...genesis.alonzo,
    executionPrices: p.executionUnitPrices,
    maxTxExUnits: p.maxTxExecutionUnits,
    maxBlockExUnits: p.maxBlockExecutionUnits,
    maxValueSize: p.maxValueSize,
    collateralPercentage: p.collateralPercentage,
    maxCollateralInputs: p.maxCollateralInputs,
    // Babbage derives coins per byte by dividing this historical value by eight.
    lovelacePerUTxOWord: p.utxoCostPerByte * 8,
    // Preserve historical cost-model arities in genesis's era-specific fields.
    // Node 11.1.0 applies the current models through this supported override.
    extraConfig: { ...genesis.alonzo.extraConfig, costModels: p.costModels },
  };
  const conway = { ...genesis.conway };
  conway.committee = {
    ...conway.committee,
    // Generated credentials start in epoch zero and obey the current term limit.
    members: Object.fromEntries(
      Object.keys(conway.committee.members).map((credential) => [
        credential,
        p.committeeMaxTermLength,
      ]),
    ),
  };
  for (const key of [
    "poolVotingThresholds",
    "dRepVotingThresholds",
    "committeeMinSize",
    "committeeMaxTermLength",
    "govActionLifetime",
    "govActionDeposit",
    "dRepDeposit",
    "dRepActivity",
    "minFeeRefScriptCostPerByte",
  ])
    conway[key] = p[key];
  conway.extraConfig = {
    ...conway.extraConfig,
    initialDReps: {
      data: Object.fromEntries(
        Object.entries(conway.extraConfig.initialDReps.data).map(
          ([id, drep]) => [id, { ...drep, deposit: p.dRepDeposit }],
        ),
      ),
    },
  };
  const byron = {
    ...genesis.byron,
    protocolConsts: {
      ...genesis.byron.protocolConsts,
      k: profile.consensus.securityParam,
    },
  };
  return { shelley, alonzo, conway, byron };
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const directory = process.argv[2];
  if (!directory)
    throw new Error("Usage: apply-configuration.mjs <genesis directory>");
  const profile = JSON.parse(
    await readFile(new URL("configuration.json", import.meta.url), "utf8"),
  );
  const genesis = {};
  for (const era of ["shelley", "alonzo", "conway", "byron"])
    genesis[era] = JSON.parse(
      await readFile(resolve(directory, `${era}-genesis.json`), "utf8"),
    );
  for (const [era, value] of Object.entries(
    applyPreprodConfiguration(genesis, profile),
  ))
    await writeFile(
      resolve(directory, `${era}-genesis.json`),
      `${JSON.stringify(value, null, 2)}\n`,
    );
}
