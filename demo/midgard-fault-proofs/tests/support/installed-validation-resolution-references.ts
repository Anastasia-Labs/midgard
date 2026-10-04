import {
  resolveValidationTraceDisputeDeploymentContracts,
  validationSemanticResolverGlobalIndex,
} from "../../src/index.js";
import { network } from "./emulator/blueprints.js";
import { type readBlueprint } from "./emulator/blueprints.js";
import { type buildCatalogueDeploymentInfo } from "./emulator/catalogue.js";
import { type buildMinimalFaultProofContracts } from "./emulator/contracts.js";
import {
  createRealL1TargetLucids,
  withRealL1MaxTxSize,
} from "./emulator/dispute-staging.js";
import {
  type createValidationDisputeParties,
  type stageAuthenticatedValidationDisputePublication,
} from "./emulator/dispute-staging.js";
import {
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
} from "./emulator/reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./emulator/removal-deployment.js";
export const stageInstalledValidationResolutionReferences = async ({
  fixture,
  contracts,
  catalogue,
  realBlueprint,
  emulator,
  challengerLucid,
  operator,
  challenger,
  referenceScriptPublisherLucid,
  validationDisputePublication,
}: {
  readonly fixture: {
    readonly evidence: {
      readonly oneStepArgument: {
        readonly resolverIndex: number;
        readonly semanticResolverIndex: number;
      };
    };
  };
  readonly contracts: Awaited<
    ReturnType<typeof buildMinimalFaultProofContracts>
  >;
  readonly catalogue: Awaited<ReturnType<typeof buildCatalogueDeploymentInfo>>;
  readonly realBlueprint: ReturnType<typeof readBlueprint>;
  readonly emulator: Awaited<
    ReturnType<typeof createValidationDisputeParties>
  >["emulator"];
  readonly challengerLucid: Awaited<
    ReturnType<typeof createValidationDisputeParties>
  >["challengerLucid"];
  readonly operator: Awaited<
    ReturnType<typeof createValidationDisputeParties>
  >["operator"];
  readonly challenger: Awaited<
    ReturnType<typeof createValidationDisputeParties>
  >["challenger"];
  readonly referenceScriptPublisherLucid: Awaited<
    ReturnType<typeof stageAuthenticatedValidationDisputePublication>
  >["referenceScriptPublisherLucid"];
  readonly validationDisputePublication: Awaited<
    ReturnType<typeof stageAuthenticatedValidationDisputePublication>
  >["validationDisputePublication"];
}) => {
  const resolverIndex = fixture.evidence.oneStepArgument.resolverIndex;
  const semanticResolverIndex =
    fixture.evidence.oneStepArgument.semanticResolverIndex;
  const prepareResolverContract =
    contracts.fraudProofContracts.validationTraceDispute.prepareResolvers[
      resolverIndex
    ];
  if (prepareResolverContract === undefined) {
    throw new Error("Selected validation prepare resolver is not deployed");
  }
  const prepareResolverPublication = await withRealL1MaxTxSize(emulator, () =>
    publishPlainReferenceScriptUtxo({
      lucid: referenceScriptPublisherLucid,
      script: prepareResolverContract.spendingScript,
      label: "validation prepare resolver",
    }),
  );
  // Resolve the selected semantic resolver through the very helper the submit
  // path uses so the published body is byte-identical to the hash-checked one.
  const interimDeploymentInfo = buildRemovalDeploymentInfo(
    contracts,
    catalogue,
    { validationDisputePublication },
  );
  const semanticContract = (
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint: realBlueprint,
      deploymentInfo: interimDeploymentInfo,
      network,
    })
  ).contracts.validationTraceDispute.semanticResolvers[
    validationSemanticResolverGlobalIndex(resolverIndex, semanticResolverIndex)
  ];
  if (semanticContract === undefined) {
    throw new Error("Selected validation semantic resolver is not deployed");
  }
  const semanticPublication = await withRealL1MaxTxSize(emulator, () =>
    publishPlainReferenceScriptUtxo({
      lucid: referenceScriptPublisherLucid,
      script: semanticContract.spendingScript,
      label: "validation semantic resolver",
    }),
  );
  const canonicalPublications: Record<
    string,
    {
      scriptHash: string;
      utxo: Awaited<ReturnType<typeof publishPlainReferenceScriptUtxo>>["utxo"];
    }
  > = {};
  if (resolverIndex === 0 && semanticResolverIndex === 1) {
    const stages = (
      await resolveValidationTraceDisputeDeploymentContracts({
        blueprint: realBlueprint,
        deploymentInfo: interimDeploymentInfo,
        network,
      })
    ).contracts.validationTraceDispute.canonicalDecodeItemStages;
    for (const [role, contract] of Object.entries(stages)) {
      const publication = await withRealL1MaxTxSize(emulator, () =>
        publishPlainReferenceScriptUtxo({
          lucid: referenceScriptPublisherLucid,
          script: contract.spendingScript,
          label: `canonical ${role}`,
        }),
      );
      canonicalPublications[`validationCanonical${role}`] = {
        scriptHash: contract.spendingScriptHash,
        utxo: publication.utxo,
      };
    }
  }
  const { targetOperatorLucid, targetChallengerLucid } =
    await createRealL1TargetLucids({
      emulator,
      sourceLucid: challengerLucid,
      operatorSeedPhrase: operator.seedPhrase,
      challengerSeedPhrase: challenger.seedPhrase,
    });
  const removal = await publishRemovalReferenceScripts({
    lucid: targetOperatorLucid,
    contracts,
  });
  const builtDeploymentInfo = buildRemovalDeploymentInfo(contracts, catalogue, {
    validationDisputePublication,
    removalReferenceScripts: removal.published,
    validationValueAndMintSemanticReferences: [
      {
        semanticResolverIndex,
        scriptHash: semanticContract.spendingScriptHash,
        utxo: semanticPublication.utxo,
      },
    ],
  });
  // The boundary-selected prepare resolver has no canonical deployment-entry
  // name; the workflow engine resolves its publication by immutable script
  // hash across the manifest entries (ruling R4), so the entry name below is
  // only a label.
  const deploymentInfo = {
    ...builtDeploymentInfo,
    contracts: {
      ...builtDeploymentInfo.contracts,
      validationSelectedSemantic: {
        scriptHash: semanticContract.spendingScriptHash,
        refScriptUTxO: {
          txHash: semanticPublication.utxo.txHash,
          outputIndex: semanticPublication.utxo.outputIndex,
        },
      },
      ...Object.fromEntries(
        Object.entries(canonicalPublications).map(
          ([name, { scriptHash, utxo }]) => [
            name,
            {
              scriptHash,
              refScriptUTxO: {
                txHash: utxo.txHash,
                outputIndex: utxo.outputIndex,
              },
            },
          ],
        ),
      ),
      validationTraceDisputeValueAndMintPrepare: {
        scriptHash: prepareResolverContract.spendingScriptHash,
        refScriptUTxO: {
          txHash: prepareResolverPublication.utxo.txHash,
          outputIndex: prepareResolverPublication.utxo.outputIndex,
        },
      },
    },
  };
  const resolvedContracts =
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      requireStateQueueMint: true,
      requireFraudProofSpend: true,
    });
  return {
    targetOperatorLucid,
    targetChallengerLucid,
    removal,
    deploymentInfo,
    resolvedContracts,
    semanticPublication,
    semanticContract,
    prepareResolverPublication,
    prepareResolverContract,
    canonicalPublications,
  };
};
