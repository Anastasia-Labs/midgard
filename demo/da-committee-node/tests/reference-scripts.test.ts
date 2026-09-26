import { referenceScriptAuthUnit } from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  mintingPolicyToId,
  type UTxO,
  validatorToAddress,
  validatorToRewardAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  daAttestationValidatorsFromDeployment,
  type MidgardDeploymentContract,
  type MidgardNodeDeployment,
} from "../src/l1/deployment.js";
import {
  type DaAttestationReferenceScripts,
  fetchDaAttestationReferenceScripts,
} from "../src/l1/reference-scripts.js";
import { loadDaDeploymentFixture } from "./helpers/deployment-fixture.js";

/**
 * The published role -> deployment-contract mapping of
 * `DaAttestationReferenceScripts`. This is the resolver's CONTRACT (which
 * deployed script each named field must carry), not a copy of its
 * implementation: the resolver's own ordering is an internal detail, and the
 * expectations below are built by looking each role's contract up here and
 * hashing its script with lucid, never by reading the resolver's output.
 */
const REFERENCE_SCRIPT_ROLE_CONTRACTS: Readonly<
  Record<
    keyof DaAttestationReferenceScripts,
    (deployment: MidgardNodeDeployment) => MidgardDeploymentContract
  >
> = {
  availabilityChallengeMinting: (d) => d.availabilityChallenge.mint,
  availabilityChallengeBondWithdrawal: (d) =>
    d.availabilityChallengeYields.bond,
  daAttestationMinting: (d) => d.daAttestation.mint,
  daAttestationSpending: (d) => d.daAttestation.spend,
  stateQueueMinting: (d) => d.stateQueue.mint,
  stateQueueSpending: (d) => d.stateQueue.spend,
};

describe("DA attestation reference script resolver", () => {
  it("binds every role to its own deployed script, independently of provider order", async () => {
    const deployment = await loadDaDeploymentFixture("Preprod");
    const utxos = referenceScriptUtxos(deployment);
    // Reversed on purpose: the resolver must key by outRef, not by the order
    // the chain provider happens to return UTxOs in.
    const resolved = await fetchDaAttestationReferenceScripts(
      lucidWithReferenceScripts([...utxos].reverse()),
      deployment,
    );

    expect(Object.keys(resolved).sort()).toEqual(
      Object.keys(REFERENCE_SCRIPT_ROLE_CONTRACTS).sort(),
    );
    const outRefKeys = new Set<string>();
    for (const [role, contractOf] of Object.entries(
      REFERENCE_SCRIPT_ROLE_CONTRACTS,
    )) {
      const contract = contractOf(deployment);
      const utxo = resolved[role as keyof DaAttestationReferenceScripts];
      expect({
        role,
        txHash: utxo.txHash,
        outputIndex: utxo.outputIndex,
      }).toEqual({
        role,
        txHash: contract.refScriptOutRef!.txHash,
        outputIndex: contract.refScriptOutRef!.outputIndex,
      });
      // Independently recomputed from the bytes the resolved UTxO actually
      // carries: the reference script at that outRef really is this role's
      // script, not merely a UTxO sitting at the advertised address.
      expect(validatorToScriptHash(utxo.scriptRef!)).toBe(contract.scriptHash);
      outRefKeys.add(`${utxo.txHash}#${utxo.outputIndex.toString()}`);
    }
    // Six distinct reference UTxOs: a resolver that returned one UTxO under
    // several roles would collapse this set. (Script hashes are NOT distinct
    // here — the DA attestation mint and spend purposes share one script.)
    expect(outRefKeys.size).toBe(6);
  });

  it("refuses a deployment whose reference UTxO is absent from the chain", async () => {
    const deployment = await loadDaDeploymentFixture("Preprod");
    const utxos = referenceScriptUtxos(deployment);
    const missing = deployment.stateQueue.mint.refScriptOutRef!;
    await expect(
      fetchDaAttestationReferenceScripts(
        lucidWithReferenceScripts(
          utxos.filter(
            (utxo) =>
              utxo.txHash !== missing.txHash ||
              utxo.outputIndex !== missing.outputIndex,
          ),
        ),
        deployment,
      ),
    ).rejects.toThrow(
      new RegExp(
        `missing state queue minting reference script UTxO at ${missing.txHash}#${missing.outputIndex.toString()}`,
        "u",
      ),
    );
  });

  it("derives an SDK validator set whose script hashes and addresses match its own scripts", async () => {
    const deployment = await loadDaDeploymentFixture("Preprod");
    const validators = daAttestationValidatorsFromDeployment(deployment);

    expect(Object.keys(validators).sort()).toEqual([
      "availabilityChallenge",
      "daAttestation",
      "daParamsGovernor",
      "hubOracle",
      "stateQueue",
    ]);
    expect(Object.keys(validators.availabilityChallenge.yields).sort()).toEqual(
      ["bond", "close", "open", "settle", "timeout"],
    );
    expect(Object.keys(validators.stateQueue.yields).sort()).toEqual([
      "commit",
      "fraudRemoval",
      "merge",
      "unattestedTimeout",
      "unavailableTimeout",
    ]);

    // Every authenticated role must be internally coherent: the advertised
    // policy id, spending hash and spending address are all recomputed here
    // from the script bytes the validator itself carries, so a role wired to
    // another contract's identity fields cannot pass.
    const authenticated = {
      hubOracle: validators.hubOracle,
      availabilityChallenge: validators.availabilityChallenge,
      daAttestation: validators.daAttestation,
      daParamsGovernor: validators.daParamsGovernor,
      stateQueue: validators.stateQueue,
    };
    for (const [role, validator] of Object.entries(authenticated)) {
      expect({
        role,
        policyId: validator.policyId,
        spendingScriptHash: validator.spendingScriptHash,
        spendingScriptAddress: validator.spendingScriptAddress,
        mintingScriptCBOR: validator.mintingScriptCBOR,
        spendingScriptCBOR: validator.spendingScriptCBOR,
      }).toEqual({
        role,
        policyId: mintingPolicyToId(validator.mintingScript),
        spendingScriptHash: validatorToScriptHash(validator.spendingScript),
        spendingScriptAddress: validatorToAddress(
          "Preprod",
          validator.spendingScript,
        ),
        mintingScriptCBOR: validator.mintingScript.script,
        spendingScriptCBOR: validator.spendingScript.script,
      });
    }

    const withdrawals = {
      ...validators.availabilityChallenge.yields,
      ...validators.stateQueue.yields,
    };
    for (const [role, withdrawal] of Object.entries(withdrawals)) {
      expect({
        role,
        hash: withdrawal.withdrawalScriptHash,
        cbor: withdrawal.withdrawalScriptCBOR,
      }).toEqual({
        role,
        hash: validatorToScriptHash(withdrawal.withdrawalScript),
        cbor: withdrawal.withdrawalScript.script,
      });
    }
    // The ten yield scripts are ten different validators; a table that wired
    // two names to the same deployed script would be caught here.
    expect(
      new Set(Object.values(withdrawals).map((w) => w.withdrawalScriptHash))
        .size,
    ).toBe(10);
  });

  it("wires the DA-facing roles to the deployment entries that carry their names", async () => {
    const deployment = await loadDaDeploymentFixture("Preprod");
    const validators = daAttestationValidatorsFromDeployment(deployment);
    // Inverted lookup: name each returned script hash by asking the
    // deployment which contract owns it, so a cross-wired role reports the
    // other contract's key rather than silently agreeing with itself. Mint
    // and spend purposes are indexed separately because a multi-purpose
    // validator (DA attestation) deploys one script under both.
    const authenticatedDeployments = {
      hubOracle: deployment.hubOracle,
      availabilityChallenge: deployment.availabilityChallenge,
      daAttestation: deployment.daAttestation,
      daParamsGovernor: deployment.daParamsGovernor,
      stateQueue: deployment.stateQueue,
    };
    const mintKeyByHash = new Map(
      Object.values(authenticatedDeployments).map(
        (d) => [d.mint.scriptHash, d.mint.key] as const,
      ),
    );
    const spendKeyByHash = new Map(
      Object.values(authenticatedDeployments).map(
        (d) => [d.spend.scriptHash, d.spend.key] as const,
      ),
    );
    for (const [role, validator] of Object.entries({
      hubOracle: validators.hubOracle,
      availabilityChallenge: validators.availabilityChallenge,
      daAttestation: validators.daAttestation,
      daParamsGovernor: validators.daParamsGovernor,
      stateQueue: validators.stateQueue,
    })) {
      expect({
        mint: mintKeyByHash.get(validatorToScriptHash(validator.mintingScript)),
        spend: spendKeyByHash.get(
          validatorToScriptHash(validator.spendingScript),
        ),
      }).toEqual({
        mint: authenticatedDeployments[
          role as keyof typeof authenticatedDeployments
        ].mint.key,
        spend:
          authenticatedDeployments[
            role as keyof typeof authenticatedDeployments
          ].spend.key,
      });
    }
  });

  it("refuses a bond reference without its exact deployment role NFT", async () => {
    const deployment = await loadDaDeploymentFixture("Preprod");
    await expect(
      fetchDaAttestationReferenceScripts(
        lucidWithReferenceScripts(
          withReplacedUtxo(
            referenceScriptUtxos(deployment),
            deployment.availabilityChallengeYields.bond.refScriptOutRef!,
            (utxo) => ({ ...utxo, assets: { lovelace: 4_000_000n } }),
          ),
        ),
        deployment,
      ),
    ).rejects.toThrow(/exact authentication role NFT/u);
  });

  it("refuses a bond role NFT that is present in the wrong quantity", async () => {
    const deployment = await loadDaDeploymentFixture("Preprod");
    const roleUnit = referenceScriptAuthUnit(
      deployment.referenceScriptAuthPolicyId,
      "availability-challenge bond withdrawal",
    );
    await expect(
      fetchDaAttestationReferenceScripts(
        lucidWithReferenceScripts(
          withReplacedUtxo(
            referenceScriptUtxos(deployment),
            deployment.availabilityChallengeYields.bond.refScriptOutRef!,
            (utxo) => ({
              ...utxo,
              assets: { ...utxo.assets, [roleUnit]: 2n },
            }),
          ),
        ),
        deployment,
      ),
    ).rejects.toThrow(/exact authentication role NFT/u);
  });

  it("refuses initialization when the bond reward account is unregistered", async () => {
    const deployment = await loadDaDeploymentFixture("Preprod");
    // The address in the refusal is recomputed here from the bond script, so
    // the test pins which account was consulted, not merely that one was.
    const rewardAddress = validatorToRewardAddress(
      "Preprod",
      deployment.availabilityChallengeYields.bond.script,
    );
    await expect(
      fetchDaAttestationReferenceScripts(
        lucidWithReferenceScripts(referenceScriptUtxos(deployment), false),
        deployment,
      ),
    ).rejects.toThrow(
      `availability challenge bond withdrawal reward account is not registered: ${rewardAddress}`,
    );
  });

  it("fails closed when a resolved UTxO has the wrong scriptRef", async () => {
    const deployment = await loadDaDeploymentFixture("Preprod");
    const outRef = deployment.stateQueue.spend.refScriptOutRef!;
    await expect(
      fetchDaAttestationReferenceScripts(
        lucidWithReferenceScripts(
          withReplacedUtxo(
            referenceScriptUtxos(deployment),
            outRef,
            (utxo) => ({
              ...utxo,
              scriptRef: deployment.daAttestation.spend.script,
            }),
          ),
        ),
        deployment,
      ),
    ).rejects.toThrow(
      `state queue spending reference script hash mismatch at ${outRef.txHash}#${outRef.outputIndex.toString()}: expected=${deployment.stateQueue.spend.scriptHash}, actual=${deployment.daAttestation.spend.scriptHash}`,
    );
  });
});

const referenceScriptUtxos = (deployment: MidgardNodeDeployment): UTxO[] =>
  Object.entries(REFERENCE_SCRIPT_ROLE_CONTRACTS).map(([role, contractOf]) => {
    const contract = contractOf(deployment);
    const base = referenceScriptUtxo(contract.refScriptOutRef, contract.script);
    return role === "availabilityChallengeBondWithdrawal"
      ? {
          ...base,
          assets: {
            ...base.assets,
            [referenceScriptAuthUnit(
              deployment.referenceScriptAuthPolicyId,
              "availability-challenge bond withdrawal",
            )]: 1n,
          },
        }
      : base;
  });

const withReplacedUtxo = (
  utxos: readonly UTxO[],
  outRef: { readonly txHash: string; readonly outputIndex: number },
  replace: (utxo: UTxO) => UTxO,
): UTxO[] => {
  const replaced = utxos.map((utxo) =>
    utxo.txHash === outRef.txHash && utxo.outputIndex === outRef.outputIndex
      ? replace(utxo)
      : utxo,
  );
  if (replaced.every((utxo, index) => utxo === utxos[index])) {
    throw new Error("fixture outRef is absent from the reference UTxO set");
  }
  return replaced;
};

const referenceScriptUtxo = (
  outRef: { readonly txHash: string; readonly outputIndex: number } | null,
  scriptRef: UTxO["scriptRef"],
): UTxO => {
  if (outRef === null) {
    throw new Error("fixture contract is expected to carry a refScriptUTxO");
  }
  return {
    txHash: outRef.txHash,
    outputIndex: outRef.outputIndex,
    address: "addr_test1vrm9x2c9s7ccg8vlt8l2f33frf84m9e8u9d7jwsu4kkwkgg7d4u73",
    assets: { lovelace: 4_000_000n },
    scriptRef,
  } as UTxO;
};

const lucidWithReferenceScripts = (
  utxos: readonly UTxO[],
  registered = true,
): Pick<LucidEvolution, "utxosByOutRef" | "rewardAccountAt" | "config"> => ({
  config: () =>
    ({ network: "Preprod" }) as ReturnType<LucidEvolution["config"]>,
  rewardAccountAt: async () => ({ registered, rewards: 0n, poolId: null }),
  utxosByOutRef: async (outRefs) =>
    outRefs.flatMap((outRef) =>
      utxos.filter(
        (utxo) =>
          utxo.txHash === outRef.txHash &&
          utxo.outputIndex === outRef.outputIndex,
      ),
    ),
});
