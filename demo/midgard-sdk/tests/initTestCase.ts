import { Effect } from "effect";
import {
  registeredOperatorsPolicyId,
  activeOperatorsPolicyId,
  retiredOperatorsPolicyId,
  schedulerPolicyId,
  stateQueuePolicyId,
  fraudProofCataloguePolicyId,
  hubOraclePolicyId,
  escapeHatchPolicyId,
  settlementQueuePolicyId,
} from "../src/core/compiled/constants.js";
import { getMidgardValidators } from "../src/core/compiled/utils.js";
import { LucidContext } from "./service/lucidContext.js";
import { bytesToHex } from "@noble/hashes/utils";
import { sha256 } from "@noble/hashes/sha2";
import { AddressData } from "@/common.js";
import {
  InitializationParams,
  unsignedInitializationProgram,
} from "../src/initialization.js";
import { HubOracleDatum } from "../src/hub-oracle.js";
import { EmptyRootData } from "../src/linked-list.js";
import { ConfirmedState } from "../src/ledger-state.js";
import {
  addressFromHexOrBech32,
  fromText,
  getAddressDetails,
} from "@lucid-evolution/lucid";

export type InitializationResult = {
  txHash: string;
  hubOraclePolicyId: string;
  stateQueuePolicyId: string;
  schedulerPolicyId: string;
  registeredOperatorsPolicyId: string;
  activeOperatorsPolicyId: string;
  retiredOperatorsPolicyId: string;
  escapeHatchPolicyId: string;
  fraudProofCataloguePolicyId: string;
  nonceUtxoConsumed: boolean;
  validators: Effect.Effect.Success<ReturnType<typeof getMidgardValidators>>; // ← Fixed
  genesisTime: bigint;
  initialOperator: string;
};

/**
 * Test case helper for Midgard initialization
 *
 * Creates a complete Midgard initialization transaction including:
 * - Hub Oracle with complete datum
 * - All 5 LinkedList root nodes (operators + queues)
 * - Scheduler, Escape Hatch, Fraud Proof Catalogue
 *
 * @param context - Lucid test context (lucid, users, emulator)
 * @param customParams - Optional custom parameters to override defaults
 * @returns Effect wrapping initialization result
 */
export const initializationTestCase = (
  { lucid, users, emulator }: LucidContext,
  customParams?: Partial<InitializationParams>,
): Effect.Effect<InitializationResult, Error, never> => {
  return Effect.gen(function* () {
    // ========================================================================
    // Setup
    // ========================================================================

    console.log("🚀 Starting Midgard initialization...");

    // Select operator wallet (who initializes the protocol)
    lucid.selectWallet.fromSeed(users.operator.seedPhrase);

    // Get a nonce UTxO
    const address = yield* Effect.promise(() => lucid.wallet().address());
    const utxos = yield* Effect.promise(() => lucid.utxosAt(address));

    if (!utxos || utxos.length === 0) {
      throw new Error("No UTxOs found for nonce");
    }

    const nonceUtxo = utxos[0];
    console.log(`   Nonce UTxO: ${nonceUtxo.txHash}#${nonceUtxo.outputIndex}`);

    // ========================================================================
    // Get All Validators
    // ========================================================================

    const validators = yield* getMidgardValidators(lucid);
    console.log("   ✅ Validators loaded");

    const create28ByteHexHash = (input: string): string => {
      // 1. Generate the 32-byte hash buffer
      const fullHashBuffer = sha256(input);
      // 2. Truncate to the required 28 bytes
      const truncatedHashBuffer = fullHashBuffer.slice(0, 28);
      // 3. Convert to hex string (will be 56 chars)
      return bytesToHex(truncatedHashBuffer);
    };
    // ========================================================================
    // Build Hub Oracle Datum
    // ========================================================================
    const mockHashHub = create28ByteHexHash("hub-oracle-data");

    const toAikenAddress = (address: string) => {
      const details = getAddressDetails(address);

      if (!details.paymentCredential) {
        throw new Error(`Invalid address: ${address}`);
      }

      // Convert payment credential - use proper tuple typing
      const paymentCredential =
        details.paymentCredential.type === "Script"
          ? { ScriptCredential: [details.paymentCredential.hash] as [string] } // ← Tuple!
          : {
              PublicKeyCredential: [details.paymentCredential.hash] as [string],
            }; // ← Tuple!

      // Convert stake credential (if present)
      let stakeCredential = null;
      if (details.stakeCredential) {
        const cred =
          details.stakeCredential.type === "Script"
            ? { ScriptCredential: [details.stakeCredential.hash] as [string] } // ← Tuple!
            : {
                PublicKeyCredential: [details.stakeCredential.hash] as [string],
              }; // ← Tuple!
        stakeCredential = { Inline: [cred] as [typeof cred] }; // ← Tuple!
      }

      return {
        paymentCredential,
        stakeCredential,
      };
    };

    const hubOracleDatum: HubOracleDatum = {
      // Policy IDs - use directly (already hex)
      registeredOperators: registeredOperatorsPolicyId,
      activeOperators: activeOperatorsPolicyId,
      retiredOperators: retiredOperatorsPolicyId,
      scheduler: schedulerPolicyId,
      stateQueue: stateQueuePolicyId,
      fraudProofCatalogue: fraudProofCataloguePolicyId,
      fraudProof: mockHashHub,

      // Addresses - convert to Aiken format
      registeredOperatorsAddr: toAikenAddress(
        validators.registeredOperators.spendAddress ||
          validators.registeredOperators.mintAddress,
      ),
      activeOperatorsAddr: toAikenAddress(
        validators.activeOperators.spendAddress,
      ),
      retiredOperatorsAddr: toAikenAddress(
        validators.retiredOperators.spendAddress ||
          validators.retiredOperators.mintAddress,
      ),
      schedulerAddr: toAikenAddress(validators.scheduler.spendAddress),
      stateQueueAddr: toAikenAddress(validators.stateQueue.spendAddress),
      fraudProofCatalogueAddr: toAikenAddress(
        validators.fraudProofCatalogue.spendAddress,
      ),
      fraudProofAddr: toAikenAddress(
        validators.fraudProofCatalogue.spendAddress,
      ),
    };

    console.log("   ✅ Hub Oracle datum constructed");

    // ========================================================================
    // Build Initialization Parameters
    // ========================================================================

    const genesisTime = BigInt(Date.now());
    const initialOperator = "00".repeat(28); // Mock operator PKH

    const mockHash = create28ByteHexHash("state-queue-data");

    const params: InitializationParams = {
      nonceUtxo,
      genesisTime,
      initialOperator: "00".repeat(28),
      hubOracle: {
        policyId: hubOraclePolicyId,
        address: validators.hubOracle.spendAddress,
        mintScript: validators.hubOracle.mintValidator,
        datum: hubOracleDatum,
      },
      stateQueue: {
        policyId: stateQueuePolicyId,
        address: validators.stateQueue.spendAddress,
        mintScript: validators.stateQueue.mintValidator,
        dataSchema: ConfirmedState,
        data: {
          headerHash: mockHash,
          prevHeaderHash: mockHash,
          utxoRoot: bytesToHex(sha256("00".repeat(32))),
          startTime: genesisTime,
          endTime: genesisTime,
          protocolVersion: 0n,
        },
      },
      settlementQueue: {
        policyId: settlementQueuePolicyId,
        address: validators.settlementQueue.spendAddress,
        mintScript: validators.settlementQueue.mintValidator,
        dataSchema: EmptyRootData,
        data: [],
      },
      registeredOperators: {
        policyId: registeredOperatorsPolicyId,
        address: validators.registeredOperators.mintAddress,
        mintScript: validators.registeredOperators.mintValidator,
        dataSchema: EmptyRootData,
        data: [],
      },
      activeOperators: {
        policyId: activeOperatorsPolicyId,
        address: validators.activeOperators.spendAddress,
        mintScript: validators.activeOperators.mintValidator,
        dataSchema: EmptyRootData,
        data: [],
      },
      retiredOperators: {
        policyId: retiredOperatorsPolicyId,
        address: validators.retiredOperators.mintAddress,
        mintScript: validators.retiredOperators.mintValidator,
        dataSchema: EmptyRootData,
        data: [],
      },
      scheduler: {
        policyId: schedulerPolicyId,
        address: validators.scheduler.spendAddress,
        mintScript: validators.scheduler.mintValidator,
        dataSchema: EmptyRootData,
        data: [],
      },
      fraudProofCatalogue: {
        policyId: fraudProofCataloguePolicyId,
        address: validators.fraudProofCatalogue.spendAddress,
        mintScript: validators.fraudProofCatalogue.mintValidator,
        mptRootHash: "00".repeat(32),
      },

      ...customParams,
    };

    console.log("   ✅ Initialization parameters built");

    // ========================================================================
    // Build, Sign, and Submit Transaction
    // ========================================================================

    const initFlow = Effect.gen(function* () {
      console.log("   🔨 Building transaction...");

      // Build and complete initialization transaction via endpoint
      const initUnsigned = yield* unsignedInitializationProgram(lucid, params);

      console.log("   ✍️  Signing transaction...");

      // Sign transaction
      const initSigned = yield* Effect.promise(() =>
        initUnsigned.sign.withWallet().complete(),
      );

      console.log("   📡 Submitting transaction...");

      // Submit transaction
      const initHash = yield* Effect.promise(() => initSigned.submit());

      return initHash;
    });

    // Execute flow with error handling
    const txHash = yield* initFlow.pipe(
      Effect.tapError((error) =>
        Effect.log(`❌ Error initializing Midgard: ${error}`),
      ),
      Effect.map((hash) => {
        console.log(`✅ Midgard initialized successfully!`);
        console.log(`   Tx Hash: ${hash}`);
        return hash;
      }),
    );

    // ========================================================================
    // Wait for Confirmation
    // ========================================================================

    if (emulator) {
      console.log("   ⏳ Waiting for block confirmation...");
      yield* Effect.sync(() => emulator.awaitBlock(1));
      console.log("   ✅ Block confirmed!");
    }

    // ========================================================================
    // Return Result
    // ========================================================================

    return {
      txHash,
      hubOraclePolicyId,
      stateQueuePolicyId,
      schedulerPolicyId,
      registeredOperatorsPolicyId,
      activeOperatorsPolicyId,
      retiredOperatorsPolicyId,
      escapeHatchPolicyId,
      fraudProofCataloguePolicyId,
      nonceUtxoConsumed: true,
      validators, // ← Return validators for testing
      genesisTime, // ← Return for verification
      initialOperator, // ← Return for verification
    };
  });
};
