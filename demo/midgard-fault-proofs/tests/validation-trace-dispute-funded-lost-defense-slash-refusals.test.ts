/** The prover funding permit refuses a lost-defense slash whose funding
 * authority differs from the slash the builder evaluated, before it is
 * submitted: an authority naming another header, deployment, economics,
 * receipt, receipt token, operator bond, operator input, reward, fee,
 * reward address or input set than the slash's own. Each case plays a
 * two-defender case to its defender's win and presents the watcher's
 * slash under the altered authority; the workflow stalls with the refusal
 * and the operator keeps its node.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  grantLostDefenseSlashFundingAuthority,
  type LostDefenseSlashFundingAuthority,
  readLostDefenseSlashFundingAuthority,
} from "../src/workflow/funding-reservation-permit.lost-defense-slash-authority.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import { workflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import {
  type FundedMoves,
  openTwoDefendedFundedCase,
  successorWinsItsThread,
} from "./support/validation-trace-dispute-funded-moves.js";
import type { AdapterWrap } from "./support/validation-trace-dispute-funded-runner.js";
import { TIMEOUT } from "./support/validation-trace-dispute-funded-verify-source.js";

const SLASH = "validation-trace-slash-lost-defense:";
const BINDING =
  "lost-defense slash funding authority differs from its exact receipt action or economics";
const ANOTHER = "00".repeat(32);

type Terms = Omit<
  LostDefenseSlashFundingAuthority,
  | "category"
  | "transactionHash"
  | "transactionBodySha256"
  | "signedTransactionCborHex"
>;

/** What the altered authority states, from the builder's own. */
type Alter = (terms: Terms, journey: FundedMoves) => Promise<Terms> | Terms;

/** Presents the watcher's slash under the authority `alter` derives. */
const presentAltered =
  (journey: FundedMoves, alter: Alter): AdapterWrap =>
  (adapter) => ({
    ...adapter,
    preflight: async (context) => {
      const preflight = await adapter.preflight(context);
      const signed = workflowPreflightTransaction(preflight);
      if (!context.action.actionId.startsWith(SLASH) || signed === undefined)
        return preflight;
      const {
        category: _category,
        transactionHash: _hash,
        transactionBodySha256: _sha,
        signedTransactionCborHex: _cbor,
        ...terms
      } = readLostDefenseSlashFundingAuthority(signed)!;
      const { inputs, ...rest } = await alter(terms, journey);
      grantLostDefenseSlashFundingAuthority({
        ...rest,
        signed,
        inputs: (await journey.scene.h.emulator.getUtxosByOutRef(
          inputs.map(({ outRef }) => parseOwnOutRef(outRef)),
        )) as UTxO[],
      });
      return preflight;
    },
  });

/** The operator's node in the active set. */
const operatorNodes = async ({ scene, defender }: FundedMoves) =>
  await scene.h.proverLucid.utxosAtWithUnit(
    scene.h.contracts.activeOperators.spendingScriptAddress,
    scene.h.contracts.activeOperators.policyId +
      SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX +
      defender.vkey,
  );

const plusOne = (lovelace: string) => (BigInt(lovelace) + 1n).toString();

describe("the lost-defense slash refused by the prover funding permit", () => {
  it.each<{ name: string; alter: Alter; refusal: string }>([
    {
      name: "another header",
      alter: (terms) => ({ ...terms, headerHash: ANOTHER }),
      refusal: `${BINDING} (header hash)`,
    },
    {
      name: "another deployment",
      alter: (terms) => ({ ...terms, deploymentFingerprint: ANOTHER }),
      refusal: `${BINDING} (deployment fingerprint)`,
    },
    {
      name: "other economics",
      alter: (terms) => ({ ...terms, economicsPolicyDigest: ANOTHER }),
      refusal: `${BINDING} (economics policy digest)`,
    },
    {
      name: "another receipt, the operator's bond",
      alter: (terms) => ({ ...terms, receiptOutRef: terms.operatorOutRef }),
      refusal: `${BINDING} (receipt)`,
    },
    {
      name: "another operator input",
      alter: (terms) => ({ ...terms, operatorOutRef: `${ANOTHER}#0` }),
      refusal: `${BINDING} (operator input)`,
    },
    {
      name: "an operator bond below any slashable bond",
      alter: (terms) => ({ ...terms, operatorBondLovelace: "0" }),
      refusal: `${BINDING} (operator bond, exact fee, body fee)`,
    },
    {
      name: "an operator bond one lovelace over the bond it spends",
      alter: (terms) => ({
        ...terms,
        operatorBondLovelace: plusOne(terms.operatorBondLovelace),
      }),
      refusal: "fraud slash operator bond differs from its authenticated bond",
    },
    {
      name: "a reward one lovelace over",
      alter: (terms) => ({
        ...terms,
        rewardLovelace: plusOne(terms.rewardLovelace),
      }),
      refusal: `${BINDING} (reward)`,
    },
    {
      name: "a fee one lovelace over",
      alter: (terms) => ({
        ...terms,
        exactFeeLovelace: plusOne(terms.exactFeeLovelace),
      }),
      refusal: `${BINDING} (exact fee)`,
    },
    {
      name: "a reward paid to a wallet outside the deployment",
      alter: async (terms, { scene }) => ({
        ...terms,
        rewardAddress: await scene.h.funderLucid.wallet().address(),
      }),
      refusal: `${BINDING} (reward address)`,
    },
    {
      name: "an input left out",
      alter: (terms) => ({ ...terms, inputs: terms.inputs.slice(1) }),
      refusal: `${BINDING} (authority inputs)`,
    },
    {
      name: "the receipt token of another defender",
      alter: (terms) => ({
        ...terms,
        receiptUnit: terms.receiptUnit.slice(0, 56) + ANOTHER,
      }),
      refusal:
        "lost-defense slash receipt is not the deployment's receipt of our won defense",
    },
    {
      name: "a receipt token under another policy",
      alter: (terms, { scene }) => ({
        ...terms,
        receiptUnit: scene.chain.case.policyId + terms.receiptUnit.slice(56),
      }),
      refusal:
        "lost-defense slash receipt is not the deployment's receipt of our won defense",
    },
  ])(
    "refuses the slash under an authority naming $name",
    async ({ alter, refusal }) => {
      const journey = await openTwoDefendedFundedCase();
      const { dispute } = journey;
      try {
        await successorWinsItsThread(journey);
        const wrap = presentAltered(journey, alter);
        const history: string[] = [];
        for (let turns = 0; turns < 12; turns += 1) {
          const result = await dispute.turn(wrap);
          history.push(
            "reason" in result
              ? `${result.kind}: ${result.reason}`
              : result.kind,
          );
          if (result.kind !== "stalled") continue;
          expect(result.reason).toContain("funding reservation failed for");
          expect(result.reason).toContain(refusal);
          expect(await operatorNodes(journey)).toHaveLength(1);
          return;
        }
        throw new Error(`the slash was not refused: ${history.join("\n")}`);
      } finally {
        await dispute.close();
      }
    },
    TIMEOUT,
  );
});
