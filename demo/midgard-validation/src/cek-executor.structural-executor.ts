import {
  decodeMidgardCekDataListNode,
  decodeMidgardCekDataNode,
  decodeMidgardCekDataPairNode,
  decodeMidgardCekProgramBlobPreimage,
  decodeMidgardCekProgramMaterialEntry,
  decodeMidgardCekProgramSequencePreimage,
  decodeMidgardCekProgramTermPreimage,
  decodeMidgardCekProgramValuePreimage,
  encodeMidgardCekProgramMaterialEntry,
  encodeMidgardCekSequenceNode,
  encodeMidgardCekValueNode,
  type Hash32,
  hashMidgardCekBlobChunk,
  hashMidgardCekContinuationFrame,
  hashMidgardCekEnvironmentNode,
  hashMidgardCekSequenceNode,
  hashMidgardCekTermNode,
  hashMidgardCekValueNode,
  MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
  MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
  MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
  type MidgardCekContinuationFrame,
  type MidgardCekMachineState,
  type MidgardCekProgramMaterialEntry,
  type MidgardCekValueNode,
} from "@al-ft/midgard-core";

import {
  evaluateMidgardCekBlsFinal,
  evaluateMidgardCekDirectBuiltin,
  hashMidgardCekDirectValueWitness,
  hashMidgardCekRuntimeValueWitness,
  type MidgardCekBlsExpressionWitness,
  midgardCekBuiltinArgumentsFailKinds,
  type MidgardCekConstantValueWitness,
  midgardCekDirectBuiltinBudget,
  type MidgardCekDirectValueWitness,
  type MidgardCekRuntimeValueWitness,
} from "./cek-builtin.js";
import {
  decodeMidgardCekConstantWitness,
  hashMidgardCekConstantWitness,
  midgardCekConstantMemorySize,
  type MidgardCekConstantType,
} from "./cek-constant.js";
import { commitMidgardCekDataTree } from "./cek-data-tree.js";
import {
  type Bytes,
  type EnvironmentNode,
  errorSuccessor,
  exactComputeSuccessor,
  exactState,
  MACHINE_STARTUP_CPU,
  MACHINE_STARTUP_MEMORY,
  type MidgardCekConstantWitnessEntry,
  type MidgardCekSourceConstantWitness,
  type MidgardCekStructuralExecution,
  rootHex,
  sameBytes,
  type SequenceNode,
} from "./cek-executor.build-midgard-cek-execution-graph.js";
import {
  advanceMidgardCekMapConversionControl,
  buildMidgardCekMapConversionResult,
  midgardCekMapConversionControl,
  midgardCekMapPairMaterial,
} from "./cek-executor.map-conversion.js";
import {
  executeMidgardCekSemanticBuiltin,
  type MidgardCekSemanticStepMaterial,
  midgardCekSemanticTopMaterial,
} from "./cek-executor.semantic-builtins.js";
import { MidgardCekSemanticStore } from "./cek-executor.semantic-store.js";
import {
  type MidgardCekStepRecorder,
  type MidgardCekStructuralVerdict,
} from "./cek-executor.step-recorder.js";
import {
  hashMidgardCekMapConversionControl,
  midgardCekBuiltinArgumentCount,
  midgardCekBuiltinForceCount,
  type MidgardCekCoreStepWitness,
  type MidgardCekEnvironmentSummary,
  MidgardCekErrorCodes,
  type MidgardCekMapConversionControl,
  returnEmptySuccessor,
} from "./cek-machine.js";

export class StructuralExecutor {
  private state: MidgardCekMachineState;
  private readonly material = new Map<string, MidgardCekProgramMaterialEntry>();
  private readonly values = new Map<string, MidgardCekValueNode>();
  /** The witness last recorded per value root: what a value read returns. */
  private readonly constants = new Map<
    string,
    MidgardCekConstantValueWitness
  >();
  /** The direct witness per value root, kept beside a semantic one. */
  private readonly directConstants = new Map<
    string,
    MidgardCekSourceConstantWitness
  >();
  private readonly blsExpressions = new Map<
    string,
    MidgardCekBlsExpressionWitness
  >();
  private readonly sequences = new Map<string, SequenceNode>();
  private readonly store = new MidgardCekSemanticStore(
    (node) => this.addValue(node),
    (key, witness) => this.setConstant(key, witness),
  );
  private readonly mapControls = new Map<
    string,
    MidgardCekMapConversionControl
  >();
  private readonly environments = new Map<string, EnvironmentNode>();
  private readonly continuations = new Map<
    string,
    MidgardCekContinuationFrame
  >();

  constructor(
    root: Bytes,
    material: Iterable<MidgardCekProgramMaterialEntry>,
    executionIndex: bigint,
    constantWitnesses: Iterable<MidgardCekConstantWitnessEntry>,
    private readonly recorder: MidgardCekStepRecorder,
  ) {
    for (const entry of material) {
      const exact = decodeMidgardCekProgramMaterialEntry(
        encodeMidgardCekProgramMaterialEntry(entry),
      );
      const key = rootHex(exact.root);
      const prior = this.material.get(key);
      if (prior !== undefined) {
        if (
          prior.kind !== exact.kind ||
          !Buffer.from(prior.preimage).equals(exact.preimage)
        ) {
          throw new Error("CEK execution material contains a hash collision");
        }
        continue;
      }
      this.material.set(key, exact);
      if (exact.kind === "value") {
        const value = decodeMidgardCekProgramValuePreimage(exact.preimage);
        this.values.set(key, {
          kind: "constant",
          typeRoot: value.typeRoot,
          payloadRoot: value.payloadRoot,
          payloadLength: value.payloadLength,
          semanticRoot: value.semanticRoot,
          memory: value.memory,
        });
      } else if (exact.kind === "sequence") {
        this.sequences.set(
          key,
          decodeMidgardCekProgramSequencePreimage(exact.preimage),
        );
      } else if (exact.kind === "dataNode") {
        this.store.dataNodes.set(key, decodeMidgardCekDataNode(exact.preimage));
      } else if (exact.kind === "dataList") {
        this.store.dataLists.set(
          key,
          decodeMidgardCekDataListNode(exact.preimage),
        );
      } else if (exact.kind === "dataPair") {
        this.store.dataPairs.set(
          key,
          decodeMidgardCekDataPairNode(exact.preimage),
        );
      } else if (exact.kind === "blobChunk" || exact.kind === "blobBranch") {
        this.store.blobs.set(
          key,
          decodeMidgardCekProgramBlobPreimage(exact.kind, exact.preimage),
        );
      }
    }
    for (const [key, witness] of constantWitnesses) {
      const root =
        witness.kind === "constant"
          ? hashMidgardCekConstantWitness(witness.witness)
          : hashMidgardCekDirectValueWitness(witness);
      if (rootHex(root) !== key || this.values.get(key)?.kind !== "constant") {
        throw new Error(
          `CEK constant witness does not match authenticated value ${key}`,
        );
      }
      this.setConstant(key, witness);
      // A costly source constant reaches builtins through its semantic tree;
      // `computeConstant` still reveals its direct witness.
      const presented =
        witness.kind === "constant"
          ? this.store.costlySourcePresentation(root, witness)
          : null;
      if (presented !== null) this.constants.set(key, presented);
    }
    this.state = Object.freeze({
      mode: "compute",
      executionIndex,
      focusRoot: Buffer.from(root),
      environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
      continuationRoot: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
      auxiliary: 0n,
      cpu: MACHINE_STARTUP_CPU,
      memory: MACHINE_STARTUP_MEMORY,
    });
  }

  /** The machine state the next transition starts from. */
  public get machineState(): MidgardCekMachineState {
    return this.state;
  }

  public run(executionBudget?: {
    readonly cpu: bigint;
    readonly memory: bigint;
  }): MidgardCekStructuralExecution | MidgardCekStructuralVerdict {
    if (
      executionBudget !== undefined &&
      (executionBudget.cpu < 0n || executionBudget.memory < 0n)
    ) {
      throw new Error("CEK execution budget must be non-negative");
    }
    const initialState = this.state;
    while (
      this.state.mode !== "haltSuccess" &&
      this.state.mode !== "haltError"
    ) {
      this.recorder.assertWithinBound();
      this.step();
      if (
        executionBudget !== undefined &&
        (this.state.cpu > executionBudget.cpu ||
          this.state.memory > executionBudget.memory)
      ) {
        return this.recorder.finish(initialState, this.state, "budgetExceeded");
      }
    }
    return this.recorder.finish(initialState, this.state, "halted");
  }

  private record(
    witness: MidgardCekCoreStepWitness,
    post: MidgardCekMachineState,
  ): void {
    this.recorder.record(this.state, post, witness);
    this.state = post;
  }

  private term(root: Bytes) {
    const entry = this.material.get(rootHex(root));
    if (entry?.kind !== "term") {
      throw new Error(`missing authenticated CEK term ${rootHex(root)}`);
    }
    return decodeMidgardCekProgramTermPreimage(entry.preimage);
  }

  private sequence(root: Bytes, length: bigint): SequenceNode {
    if (length === 0n) {
      if (!sameBytes(root, MIDGARD_CEK_EMPTY_SEQUENCE_ROOT)) {
        throw new Error("empty CEK sequence has a non-canonical root");
      }
      throw new Error("an empty CEK sequence has no head");
    }
    const node = this.sequences.get(rootHex(root));
    if (node === undefined || node.length !== length) {
      throw new Error(`missing authenticated CEK sequence ${rootHex(root)}`);
    }
    return node;
  }

  private value(root: Bytes): MidgardCekValueNode {
    const value = this.values.get(rootHex(root));
    if (value === undefined) {
      throw new Error(`missing authenticated CEK value ${rootHex(root)}`);
    }
    return value;
  }

  private addValue(node: MidgardCekValueNode): Hash32 {
    const root = hashMidgardCekValueNode(node);
    const key = rootHex(root);
    const prior = this.values.get(key);
    if (
      prior !== undefined &&
      !Buffer.from(encodeMidgardCekValueNode(prior)).equals(
        encodeMidgardCekValueNode(node),
      )
    ) {
      throw new Error("CEK runtime value hash collision");
    }
    this.values.set(key, node);
    return root;
  }

  /**
   * Records the witness of the constant at `key`. Every witness recorded
   * here commits to its key, so the witness object itself identifies the
   * value root it was read from.
   */
  private setConstant(
    key: string,
    witness: MidgardCekConstantValueWitness,
  ): void {
    this.constants.set(key, witness);
    if (witness.kind === "constant") this.directConstants.set(key, witness);
    this.store.registerWitness(witness.witness, Buffer.from(key, "hex"));
  }

  private readonly typeOf = (
    value: MidgardCekRuntimeValueWitness,
  ): MidgardCekConstantType | null =>
    value.kind === "constant" || value.kind === "semanticConstant"
      ? this.store.typeOf(value.witness.typeCbor)
      : null;

  private readonly memoryOf = (
    witness: Extract<
      MidgardCekDirectValueWitness,
      { readonly kind: "constant" }
    >["witness"],
  ): bigint => {
    const root = this.store.witnessRoot(witness);
    if (root === undefined) {
      const decoded = decodeMidgardCekConstantWitness(witness);
      return midgardCekConstantMemorySize(decoded.type, decoded.payload);
    }
    const value = this.value(root);
    if (value.kind !== "constant") {
      throw new Error("CEK constant witness names a non-constant value");
    }
    return value.memory;
  };

  private budget(
    tag: bigint,
    arguments_: readonly MidgardCekDirectValueWitness[],
  ) {
    return midgardCekDirectBuiltinBudget(tag, arguments_, this.memoryOf);
  }

  private addDirectResult(result: MidgardCekDirectValueWitness): Bytes {
    if (result.kind === "constant" || result.kind === "semanticConstant") {
      // A builtin that selects one of its arguments returns that argument's
      // own authenticated witness.
      const known = this.store.witnessRoot(result.witness);
      if (known !== undefined) return known;
    }
    if (result.kind === "constant") {
      const witness = result.witness;
      const decoded = decodeMidgardCekConstantWitness(witness);
      const semantic = commitMidgardCekDataTree(decoded.payload);
      const resultRoot = this.addValue({
        kind: "constant",
        typeRoot: hashMidgardCekBlobChunk(witness.typeCbor),
        payloadRoot: semantic.root,
        payloadLength: semantic.cborLength,
        semanticRoot: semantic.root,
        memory: midgardCekConstantMemorySize(decoded.type, decoded.payload),
      });
      this.setConstant(rootHex(resultRoot), { kind: "constant", witness });
      return resultRoot;
    }
    const resultRoot = hashMidgardCekDirectValueWitness(result);
    if (result.kind === "semanticConstant") {
      const witness = result.witness;
      const node = {
        kind: "constant",
        typeRoot: hashMidgardCekBlobChunk(witness.typeCbor),
        payloadRoot: witness.payload.root,
        payloadLength: witness.payload.cborLength,
        semanticRoot: witness.payload.root,
        memory: witness.memory,
      } as const;
      if (!sameBytes(this.addValue(node), resultRoot)) {
        throw new Error("CEK semantic builtin result constant root mismatch");
      }
      this.setConstant(rootHex(resultRoot), result);
    } else if (result.kind === "blsMillerLoop") {
      if (
        !sameBytes(
          this.addValue({
            kind: "blsMillerLoop",
            expressionRoot: result.expressionRoot,
          }),
          resultRoot,
        )
      ) {
        throw new Error("CEK builtin BLS expression root mismatch");
      }
    } else {
      this.value(resultRoot);
    }
    return resultRoot;
  }

  private addSequence(node: SequenceNode): Hash32 {
    const root = hashMidgardCekSequenceNode(node);
    const key = rootHex(root);
    const prior = this.sequences.get(key);
    if (
      prior !== undefined &&
      !Buffer.from(encodeMidgardCekSequenceNode(prior)).equals(
        encodeMidgardCekSequenceNode(node),
      )
    ) {
      throw new Error("CEK runtime sequence hash collision");
    }
    this.sequences.set(key, node);
    return root;
  }

  private addEnvironment(node: EnvironmentNode): Hash32 {
    const root = hashMidgardCekEnvironmentNode(node);
    this.environments.set(rootHex(root), node);
    return root;
  }

  private environmentSummary(root: Bytes): MidgardCekEnvironmentSummary {
    if (sameBytes(root, MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT)) {
      return { kind: "empty" };
    }
    const node = this.environments.get(rootHex(root));
    if (node === undefined) {
      throw new Error(`missing authenticated CEK environment ${rootHex(root)}`);
    }
    return {
      kind: "nonempty",
      value: node.value,
      tail: node.tail,
      length: node.length,
    };
  }

  private addContinuation(frame: MidgardCekContinuationFrame): Hash32 {
    const root = hashMidgardCekContinuationFrame(frame);
    this.continuations.set(rootHex(root), frame);
    return root;
  }

  private continuation(root: Bytes): MidgardCekContinuationFrame | null {
    if (sameBytes(root, MIDGARD_CEK_EMPTY_CONTINUATION_ROOT)) {
      return null;
    }
    const frame = this.continuations.get(rootHex(root));
    if (frame === undefined) {
      throw new Error(
        `missing authenticated CEK continuation ${rootHex(root)}`,
      );
    }
    return frame;
  }

  private step(): void {
    switch (this.state.mode) {
      case "compute":
        this.compute();
        return;
      case "lookup":
        this.lookup();
        return;
      case "return":
        this.returnValue();
        return;
      case "caseSelect":
        this.selectCase();
        return;
      case "caseApply":
        this.applyCase();
        return;
      case "builtin":
        this.executeBuiltin();
        return;
      case "semanticBuiltin":
        this.executeSemanticBuiltinControl();
        return;
      case "haltSuccess":
      case "haltError":
        throw new Error("cannot step a halted CEK machine");
    }
  }

  private compute(): void {
    const pre = this.state;
    const term = this.term(pre.focusRoot);
    switch (term.kind) {
      case "variable":
        this.record(
          { kind: "computeVariable", index: term.index },
          exactComputeSuccessor(pre, {
            mode: "lookup",
            focusRoot: pre.environmentRoot,
            environmentRoot: pre.environmentRoot,
            continuationRoot: pre.continuationRoot,
            auxiliary: term.index,
          }),
        );
        return;
      case "constant":
        this.value(term.value);
        {
          const value = this.directConstants.get(rootHex(term.value));
          if (value === undefined) {
            throw new Error(
              "source CEK constant is missing its exact bounded direct witness",
            );
          }
          this.record(
            { kind: "computeConstant", value: value.witness },
            exactComputeSuccessor(pre, {
              mode: "return",
              focusRoot: term.value,
              environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
              continuationRoot: pre.continuationRoot,
              auxiliary: 0n,
            }),
          );
        }
        return;
      case "contextConstant":
        this.value(term.value);
        this.record(
          { kind: "computeContextConstant", valueRoot: term.value },
          exactComputeSuccessor(pre, {
            mode: "return",
            focusRoot: term.value,
            environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
            continuationRoot: pre.continuationRoot,
            auxiliary: 0n,
          }),
        );
        return;
      case "unaryTerm": {
        if (term.termKind === "lambda" || term.termKind === "delay") {
          const value = this.addValue({
            kind: term.termKind,
            body: term.child,
            environment: pre.environmentRoot,
          });
          this.record(
            term.termKind === "lambda"
              ? { kind: "computeLambda", body: term.child }
              : { kind: "computeDelay", body: term.child },
            exactComputeSuccessor(pre, {
              mode: "return",
              focusRoot: value,
              environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
              continuationRoot: pre.continuationRoot,
              auxiliary: 0n,
            }),
          );
          return;
        }
        const continuation = this.addContinuation({
          kind: "force",
          tail: pre.continuationRoot,
        });
        this.record(
          { kind: "computeForce", term: term.child },
          exactComputeSuccessor(pre, {
            mode: "compute",
            focusRoot: term.child,
            environmentRoot: pre.environmentRoot,
            continuationRoot: continuation,
            auxiliary: 0n,
          }),
        );
        return;
      }
      case "application": {
        const continuation = this.addContinuation({
          kind: "applyArgument",
          argument: term.argument,
          environment: pre.environmentRoot,
          tail: pre.continuationRoot,
        });
        this.record(
          {
            kind: "computeApplication",
            function: term.function,
            argument: term.argument,
          },
          exactComputeSuccessor(pre, {
            mode: "compute",
            focusRoot: term.function,
            environmentRoot: pre.environmentRoot,
            continuationRoot: continuation,
            auxiliary: 0n,
          }),
        );
        return;
      }
      case "error":
        this.record(
          { kind: "computeError" },
          errorSuccessor(pre, MidgardCekErrorCodes.Explicit),
        );
        return;
      case "builtin": {
        const value = this.addValue({
          kind: "builtin",
          tag: term.tag,
          forcesRemaining: midgardCekBuiltinForceCount(term.tag),
          argumentsCount: 0n,
          argumentsRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
        });
        this.record(
          { kind: "computeBuiltin", tag: term.tag },
          exactComputeSuccessor(pre, {
            mode: "return",
            focusRoot: value,
            environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
            continuationRoot: pre.continuationRoot,
            auxiliary: 0n,
          }),
        );
        return;
      }
      case "constr": {
        if (term.count === 0n) {
          if (!sameBytes(term.sequence, MIDGARD_CEK_EMPTY_SEQUENCE_ROOT)) {
            throw new Error("empty CEK constructor has a non-canonical root");
          }
          const value = this.addValue({
            kind: "constr",
            tag: term.tag,
            valuesCount: 0n,
            valuesRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
          });
          this.record(
            { kind: "computeConstrEmpty", tag: term.tag },
            exactComputeSuccessor(pre, {
              mode: "return",
              focusRoot: value,
              environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
              continuationRoot: pre.continuationRoot,
              auxiliary: 0n,
            }),
          );
          return;
        }
        const sequence = this.sequence(term.sequence, term.count);
        const continuation = this.addContinuation({
          kind: "constr",
          tag: term.tag,
          remainingTermsCount: term.count - 1n,
          remainingTermsRoot: sequence.tail,
          valuesCount: 0n,
          valuesRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
          environment: pre.environmentRoot,
          tail: pre.continuationRoot,
        });
        this.record(
          {
            kind: "computeConstrNonempty",
            tag: term.tag,
            termsCount: term.count,
            firstTerm: sequence.head,
            remainingTermsRoot: sequence.tail,
          },
          exactComputeSuccessor(pre, {
            mode: "compute",
            focusRoot: sequence.head,
            environmentRoot: pre.environmentRoot,
            continuationRoot: continuation,
            auxiliary: 0n,
          }),
        );
        return;
      }
      case "case": {
        if (term.count > 0n) this.sequence(term.sequence, term.count);
        if (
          term.count === 0n &&
          !sameBytes(term.sequence, MIDGARD_CEK_EMPTY_SEQUENCE_ROOT)
        ) {
          throw new Error("empty CEK case has a non-canonical branch root");
        }
        const continuation = this.addContinuation({
          kind: "case",
          branchesCount: term.count,
          branchesRoot: term.sequence,
          environment: pre.environmentRoot,
          tail: pre.continuationRoot,
        });
        this.record(
          {
            kind: "computeCase",
            scrutinee: term.scrutinee,
            branchesCount: term.count,
            branchesRoot: term.sequence,
          },
          exactComputeSuccessor(pre, {
            mode: "compute",
            focusRoot: term.scrutinee,
            environmentRoot: pre.environmentRoot,
            continuationRoot: continuation,
            auxiliary: 0n,
          }),
        );
        return;
      }
    }
  }

  private lookup(): void {
    const pre = this.state;
    if (sameBytes(pre.focusRoot, MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT)) {
      this.record(
        { kind: "lookupEmptyEnvironment" },
        errorSuccessor(pre, MidgardCekErrorCodes.UnboundVariable),
      );
      return;
    }
    const node = this.environments.get(rootHex(pre.focusRoot));
    if (node === undefined) {
      throw new Error(
        `missing authenticated CEK environment ${rootHex(pre.focusRoot)}`,
      );
    }
    this.record(
      {
        kind: "lookupEnvironment",
        value: node.value,
        tail: node.tail,
        length: node.length,
      },
      pre.auxiliary === 0n
        ? exactState(pre, {
            mode: "return",
            focusRoot: node.value,
            environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
            continuationRoot: pre.continuationRoot,
            auxiliary: 0n,
          })
        : exactState(pre, {
            mode: "lookup",
            focusRoot: node.tail,
            environmentRoot: node.tail,
            continuationRoot: pre.continuationRoot,
            auxiliary: pre.auxiliary - 1n,
          }),
    );
  }

  private applyBuiltin(
    pre: MidgardCekMachineState,
    argument: Bytes,
    builtin: Extract<MidgardCekValueNode, { readonly kind: "builtin" }>,
    tail: Bytes,
  ): MidgardCekMachineState {
    const required = midgardCekBuiltinArgumentCount(builtin.tag);
    if (builtin.forcesRemaining !== 0n || builtin.argumentsCount >= required) {
      throw new Error(
        `invalid CEK builtin application state tag=${builtin.tag.toString()} forces=${builtin.forcesRemaining.toString()} arguments=${builtin.argumentsCount.toString()} required=${required.toString()}`,
      );
    }
    const nextCount = builtin.argumentsCount + 1n;
    const nextRoot = this.addSequence({
      head: Buffer.from(argument) as Hash32,
      tail: Buffer.from(builtin.argumentsRoot) as Hash32,
      length: nextCount,
    });
    const nextValue = this.addValue({
      ...builtin,
      argumentsCount: nextCount,
      argumentsRoot: nextRoot,
    });
    return exactState(pre, {
      mode: nextCount === required ? "builtin" : "return",
      focusRoot: nextValue,
      environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
      continuationRoot: tail,
      auxiliary: 0n,
    });
  }

  private runtimeValueWitness(root: Bytes): MidgardCekRuntimeValueWitness {
    const value = this.value(root);
    switch (value.kind) {
      case "constant": {
        const witness = this.constants.get(rootHex(root));
        if (witness === undefined) {
          throw new Error(
            `missing authenticated CEK constant preimage ${rootHex(root)}`,
          );
        }
        return witness;
      }
      case "lambda":
        return {
          kind: "lambda",
          body: value.body,
          environment: value.environment,
        };
      case "delay":
        return {
          kind: "delay",
          body: value.body,
          environment: value.environment,
        };
      case "constr":
        return {
          kind: "constr",
          tag: value.tag,
          valuesCount: value.valuesCount,
          valuesRoot: value.valuesRoot,
        };
      case "builtin":
        return {
          kind: "builtin",
          tag: value.tag,
          forcesRemaining: value.forcesRemaining,
          argumentsCount: value.argumentsCount,
          argumentsRoot: value.argumentsRoot,
        };
      case "blsMillerLoop":
        return {
          kind: "blsMillerLoop",
          expressionRoot: value.expressionRoot,
        };
    }
  }

  private builtinArguments(
    root: Bytes,
    count: bigint,
  ): {
    readonly witnesses: readonly MidgardCekRuntimeValueWitness[];
    readonly roots: readonly Bytes[];
  } {
    const witnesses: MidgardCekRuntimeValueWitness[] = [];
    const roots: Bytes[] = [];
    let cursor = Buffer.from(root);
    let remaining = count;
    while (remaining > 0n) {
      const node = this.sequence(cursor, remaining);
      const witness = this.runtimeValueWitness(node.head);
      // A constant's witness is recorded only under the root it commits to;
      // every other witness is rebuilt from its value node here.
      if (
        witness.kind !== "constant" &&
        witness.kind !== "semanticConstant" &&
        !sameBytes(hashMidgardCekRuntimeValueWitness(witness), node.head)
      ) {
        throw new Error("CEK runtime argument witness root mismatch");
      }
      witnesses.push(witness);
      roots.push(node.head);
      cursor = Buffer.from(node.tail);
      remaining -= 1n;
    }
    if (!sameBytes(cursor, MIDGARD_CEK_EMPTY_SEQUENCE_ROOT)) {
      throw new Error("CEK builtin argument sequence has a non-empty tail");
    }
    return {
      witnesses: Object.freeze(witnesses.reverse()),
      roots: Object.freeze(roots.reverse()),
    };
  }

  private startMapConversion(
    pre: MidgardCekMachineState,
    tag: 38n | 43n,
    arguments_: readonly MidgardCekDirectValueWitness[],
    roots: readonly Bytes[],
  ): void {
    const source = arguments_[0];
    if (source?.kind !== "constant" && source?.kind !== "semanticConstant") {
      throw new Error("CEK map conversion requires a constant source");
    }
    const sourceRoot = this.store.constant(roots[0]!, source).dataRoot;
    const added = buildMidgardCekMapConversionResult(
      this.store,
      tag,
      sourceRoot,
    );
    const { sourceNode, resultNode } = added;
    const control = midgardCekMapConversionControl(
      tag,
      added.root,
      sourceNode,
      resultNode,
      this.budget(tag, arguments_),
    );
    const controlRoot = hashMidgardCekMapConversionControl(control);
    this.mapControls.set(rootHex(controlRoot), control);
    const { dataLists, dataPairs } = this.store;
    this.record(
      {
        kind: "startBuiltinMapConversion",
        tag,
        arguments: arguments_,
        result: added.result,
        material: {
          sourceNode,
          sourceList:
            sourceNode.kind === "list" && sourceNode.itemsCount > 0n
              ? (dataLists.get(rootHex(sourceNode.itemsRoot)) ?? null)
              : null,
          sourcePairs:
            sourceNode.kind === "map" && sourceNode.entriesCount > 0n
              ? (dataPairs.get(rootHex(sourceNode.entriesRoot)) ?? null)
              : null,
          resultNode,
          resultList:
            resultNode.kind === "list" && resultNode.itemsCount > 0n
              ? (dataLists.get(rootHex(resultNode.itemsRoot)) ?? null)
              : null,
          resultPairs:
            resultNode.kind === "map" && resultNode.entriesCount > 0n
              ? (dataPairs.get(rootHex(resultNode.entriesRoot)) ?? null)
              : null,
        },
      },
      exactState(pre, {
        mode: "semanticBuiltin",
        focusRoot: controlRoot,
        environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
        continuationRoot: pre.continuationRoot,
        auxiliary: 0n,
      }),
    );
  }

  private executeSemanticBuiltinControl(): void {
    const pre = this.state;
    const control = this.mapControls.get(rootHex(pre.focusRoot));
    if (control === undefined) {
      throw new Error("missing authenticated CEK semantic control");
    }
    if (control.sourceRemaining === 0n) {
      this.record(
        { kind: "finishBuiltinMapConversion", control },
        exactState(pre, {
          mode: "return",
          focusRoot: control.resultRoot,
          environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
          continuationRoot: pre.continuationRoot,
          auxiliary: 0n,
          cpuDelta: control.budgetCpu,
          memoryDelta: control.budgetMemory,
        }),
      );
      return;
    }
    if (control.tag === 38n) {
      const source = this.store.dataLists.get(rootHex(control.sourceRoot));
      const destination = this.store.dataPairs.get(
        rootHex(control.destinationRoot),
      );
      if (source === undefined || destination === undefined) {
        throw new Error("CEK list-to-map control material is missing");
      }
      const material = midgardCekMapPairMaterial(this.store, source.head);
      const next = advanceMidgardCekMapConversionControl(
        control,
        source.headCborLength,
        source.headMemory,
        source.tail,
        destination.keyCborLength + destination.valueCborLength,
        destination.keyMemory + destination.valueMemory,
        destination.tail,
      );
      const nextRoot = hashMidgardCekMapConversionControl(next);
      this.mapControls.set(rootHex(nextRoot), next);
      this.record(
        {
          kind: "stepBuiltinListToMap",
          control,
          source,
          ...material,
          destination,
        },
        exactState(pre, {
          mode: "semanticBuiltin",
          focusRoot: nextRoot,
          environmentRoot: pre.environmentRoot,
          continuationRoot: pre.continuationRoot,
          auxiliary: 0n,
        }),
      );
      return;
    }
    const source = this.store.dataPairs.get(rootHex(control.sourceRoot));
    const destination = this.store.dataLists.get(
      rootHex(control.destinationRoot),
    );
    if (source === undefined || destination === undefined) {
      throw new Error("CEK map-to-list control material is missing");
    }
    const material = midgardCekMapPairMaterial(this.store, destination.head);
    const next = advanceMidgardCekMapConversionControl(
      control,
      source.keyCborLength + source.valueCborLength,
      source.keyMemory + source.valueMemory,
      source.tail,
      destination.headCborLength,
      destination.headMemory,
      destination.tail,
    );
    const nextRoot = hashMidgardCekMapConversionControl(next);
    this.mapControls.set(rootHex(nextRoot), next);
    this.record(
      {
        kind: "stepBuiltinMapToList",
        control,
        source,
        destination,
        ...material,
      },
      exactState(pre, {
        mode: "semanticBuiltin",
        focusRoot: nextRoot,
        environmentRoot: pre.environmentRoot,
        continuationRoot: pre.continuationRoot,
        auxiliary: 0n,
      }),
    );
  }

  private recordSemanticResult(
    pre: MidgardCekMachineState,
    tag: bigint,
    arguments_: readonly MidgardCekDirectValueWitness[],
    result: MidgardCekDirectValueWitness,
    resultRoot: Bytes,
    material: MidgardCekSemanticStepMaterial,
  ): void {
    const budget = this.budget(tag, arguments_);
    this.record(
      {
        kind: "executeBuiltinSemantic",
        tag,
        arguments: arguments_,
        result,
        material,
      },
      exactState(pre, {
        mode: "return",
        focusRoot: resultRoot,
        environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
        continuationRoot: pre.continuationRoot,
        auxiliary: 0n,
        cpuDelta: budget.cpu,
        memoryDelta: budget.memory,
      }),
    );
  }

  private recordSemanticFailure(
    pre: MidgardCekMachineState,
    tag: bigint,
    arguments_: readonly MidgardCekDirectValueWitness[],
    material: MidgardCekSemanticStepMaterial,
  ): void {
    this.record(
      {
        kind: "executeBuiltinSemanticFailure",
        tag,
        arguments: arguments_,
        material,
      },
      errorSuccessor(pre, MidgardCekErrorCodes.BuiltinFailure),
    );
  }

  private executeSemanticBuiltin(
    pre: MidgardCekMachineState,
    tag: bigint,
    arguments_: readonly MidgardCekDirectValueWitness[],
    roots: readonly Bytes[],
  ): void {
    const outcome = executeMidgardCekSemanticBuiltin(
      this.store,
      tag,
      arguments_,
      roots,
    );
    if (outcome.kind === "failure") {
      this.recordSemanticFailure(
        pre,
        tag,
        outcome.arguments ?? arguments_,
        outcome.material,
      );
      return;
    }
    this.recordSemanticResult(
      pre,
      tag,
      arguments_,
      outcome.result,
      outcome.root ?? this.addDirectResult(outcome.result),
      outcome.material,
    );
  }

  private executeBuiltin(): void {
    const pre = this.state;
    const builtin = this.value(pre.focusRoot);
    if (
      builtin.kind !== "builtin" ||
      builtin.forcesRemaining !== 0n ||
      builtin.argumentsCount !== midgardCekBuiltinArgumentCount(builtin.tag)
    ) {
      throw new Error("CEK builtin mode has an incomplete builtin value");
    }
    const { witnesses: arguments_, roots } = this.builtinArguments(
      builtin.argumentsRoot,
      builtin.argumentsCount,
    );
    // The builtin value commits to exactly these argument roots, so only the
    // argument kinds decide a type failure here, at any argument size, as on
    // L1.
    if (
      midgardCekBuiltinArgumentsFailKinds(builtin.tag, arguments_, this.typeOf)
    ) {
      this.record(
        {
          kind: "executeBuiltinTypeFailure",
          tag: builtin.tag,
          arguments: arguments_,
        },
        errorSuccessor(pre, MidgardCekErrorCodes.BuiltinFailure),
      );
      return;
    }
    const directArguments = arguments_.map(
      (argument, index): MidgardCekDirectValueWitness => {
        if (argument.kind === "constant") {
          return { kind: "constant", witness: argument.witness };
        }
        if (argument.kind === "semanticConstant") {
          return {
            kind: "semanticConstant",
            witness: argument.witness,
          };
        }
        if (argument.kind === "blsMillerLoop") {
          return {
            kind: "blsMillerLoop",
            expressionRoot: argument.expressionRoot,
          };
        }
        return { kind: "opaque", root: roots[index]! };
      },
    );
    if (
      (builtin.tag === 38n || builtin.tag === 43n) &&
      (directArguments[0]?.kind === "constant" ||
        directArguments[0]?.kind === "semanticConstant")
    ) {
      // `mapData`/`unMapData` always run through the pair-by-pair
      // map-conversion micro-machine, whatever the source constant's witness
      // kind: the L1 rule is constant-cost and total for every map or
      // pair-list width including empty, and only `unMapData` on a non-map
      // fails (the "Expected the Map constructor" failure).
      if (builtin.tag === 43n) {
        const source = this.store.constant(roots[0]!, directArguments[0]);
        if (this.store.node(source.dataRoot).kind !== "map") {
          this.recordSemanticFailure(
            pre,
            builtin.tag,
            directArguments,
            midgardCekSemanticTopMaterial(this.store, source.dataRoot),
          );
          return;
        }
      }
      this.startMapConversion(pre, builtin.tag, directArguments, roots);
      return;
    }
    if (
      builtin.tag === 51n &&
      directArguments[0]?.kind === "semanticConstant"
    ) {
      this.executeSemanticBuiltin(pre, builtin.tag, directArguments, roots);
      return;
    }
    if (
      builtin.tag >= 29n &&
      builtin.tag <= 50n &&
      builtin.tag !== 38n &&
      builtin.tag !== 43n
    ) {
      this.executeSemanticBuiltin(pre, builtin.tag, directArguments, roots);
      return;
    }
    if (builtin.tag === 70n) {
      const left = directArguments[0];
      const right = directArguments[1];
      if (left?.kind !== "blsMillerLoop" || right?.kind !== "blsMillerLoop") {
        throw new Error(
          "CEK BLS finalVerify requires two authenticated expression values",
        );
      }
      const leftExpression = this.blsExpressions.get(
        rootHex(left.expressionRoot),
      );
      const rightExpression = this.blsExpressions.get(
        rootHex(right.expressionRoot),
      );
      if (leftExpression === undefined || rightExpression === undefined) {
        throw new Error(
          "CEK BLS finalVerify is missing an authenticated expression witness",
        );
      }
      const evaluated = evaluateMidgardCekBlsFinal(
        left.expressionRoot,
        right.expressionRoot,
        leftExpression,
        rightExpression,
      );
      const resultRoot = this.addDirectResult(evaluated.result);
      this.record(
        {
          kind: "executeBuiltinBlsFinal",
          leftRoot: left.expressionRoot,
          rightRoot: right.expressionRoot,
          leftExpression,
          rightExpression,
          result: evaluated.result,
        },
        exactState(pre, {
          mode: "return",
          focusRoot: resultRoot,
          environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
          continuationRoot: pre.continuationRoot,
          auxiliary: 0n,
          cpuDelta: evaluated.budget.cpu,
          memoryDelta: evaluated.budget.memory,
        }),
      );
      return;
    }
    const evaluated = evaluateMidgardCekDirectBuiltin(
      builtin.tag,
      directArguments,
    );
    if (evaluated.kind === "failure") {
      this.record(
        {
          kind: "executeBuiltinFailure",
          tag: builtin.tag,
          arguments: directArguments,
        },
        exactState(pre, {
          mode: "haltError",
          focusRoot: hashMidgardCekTermNode({ kind: "error" }),
          environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
          continuationRoot: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
          auxiliary: MidgardCekErrorCodes.BuiltinFailure,
          cpuDelta: evaluated.budget.cpu,
          memoryDelta: evaluated.budget.memory,
        }),
      );
      return;
    }
    if (builtin.tag === 51n && evaluated.result.kind === "semanticConstant") {
      this.executeSemanticBuiltin(pre, builtin.tag, directArguments, roots);
      return;
    }
    const resultRoot = this.addDirectResult(evaluated.result);
    if (builtin.tag === 68n) {
      const left = directArguments[0];
      const right = directArguments[1];
      if (
        evaluated.result.kind !== "blsMillerLoop" ||
        left?.kind !== "constant" ||
        right?.kind !== "constant"
      ) {
        throw new Error("CEK millerLoop returned an invalid expression value");
      }
      this.blsExpressions.set(
        rootHex(evaluated.result.expressionRoot),
        Object.freeze({
          kind: "millerLoop",
          g1: left.witness,
          g2: right.witness,
        }),
      );
    } else if (builtin.tag === 69n) {
      const left = directArguments[0];
      const right = directArguments[1];
      if (
        evaluated.result.kind !== "blsMillerLoop" ||
        left?.kind !== "blsMillerLoop" ||
        right?.kind !== "blsMillerLoop"
      ) {
        throw new Error("CEK mulMlResult returned an invalid expression value");
      }
      const leftExpression = this.blsExpressions.get(
        rootHex(left.expressionRoot),
      );
      const rightExpression = this.blsExpressions.get(
        rootHex(right.expressionRoot),
      );
      if (leftExpression === undefined || rightExpression === undefined) {
        throw new Error(
          "CEK mulMlResult is missing an authenticated child expression",
        );
      }
      this.blsExpressions.set(
        rootHex(evaluated.result.expressionRoot),
        Object.freeze({
          kind: "multiply",
          left: leftExpression,
          right: rightExpression,
        }),
      );
    }
    this.record(
      {
        kind: "executeBuiltinDirect",
        tag: builtin.tag,
        arguments: directArguments,
        result: evaluated.result,
      },
      exactState(pre, {
        mode: "return",
        focusRoot: resultRoot,
        environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
        continuationRoot: pre.continuationRoot,
        auxiliary: 0n,
        cpuDelta: evaluated.budget.cpu,
        memoryDelta: evaluated.budget.memory,
      }),
    );
  }

  private returnValue(): void {
    const pre = this.state;
    const value = this.value(pre.focusRoot);
    const frame = this.continuation(pre.continuationRoot);
    if (frame === null) {
      this.record(
        { kind: "returnEmptyContinuation", value },
        returnEmptySuccessor(pre, value),
      );
      return;
    }
    switch (frame.kind) {
      case "applyArgument": {
        const continuation = this.addContinuation({
          kind: "applyFunction",
          functionValue: pre.focusRoot,
          tail: frame.tail,
        });
        this.record(
          {
            kind: "returnApplyArgument",
            argument: frame.argument,
            capturedEnvironment: frame.environment,
            tail: frame.tail,
          },
          exactState(pre, {
            mode: "compute",
            focusRoot: frame.argument,
            environmentRoot: frame.environment,
            continuationRoot: continuation,
            auxiliary: 0n,
          }),
        );
        return;
      }
      case "applyFunction": {
        const functionValue = this.value(frame.functionValue);
        if (functionValue.kind === "lambda") {
          const summary = this.environmentSummary(functionValue.environment);
          const environment = this.addEnvironment({
            value: Buffer.from(pre.focusRoot) as Hash32,
            tail: Buffer.from(functionValue.environment) as Hash32,
            length: (summary.kind === "empty" ? 0n : summary.length) + 1n,
          });
          this.record(
            {
              kind: "returnApplyLambda",
              body: functionValue.body,
              closureEnvironment: functionValue.environment,
              closureSummary: summary,
              tail: frame.tail,
            },
            exactState(pre, {
              mode: "compute",
              focusRoot: functionValue.body,
              environmentRoot: environment,
              continuationRoot: frame.tail,
              auxiliary: 0n,
            }),
          );
          return;
        }
        if (functionValue.kind === "builtin") {
          this.record(
            {
              kind: "returnApplyBuiltin",
              tag: functionValue.tag,
              forcesRemaining: functionValue.forcesRemaining,
              argumentsCount: functionValue.argumentsCount,
              argumentsRoot: functionValue.argumentsRoot,
              tail: frame.tail,
            },
            this.applyBuiltin(pre, pre.focusRoot, functionValue, frame.tail),
          );
          return;
        }
        this.record(
          {
            kind: "returnApplyInvalid",
            function: functionValue,
            tail: frame.tail,
          },
          errorSuccessor(pre, MidgardCekErrorCodes.InvalidApplication),
        );
        return;
      }
      case "applyValue": {
        if (value.kind === "lambda") {
          const summary = this.environmentSummary(value.environment);
          const environment = this.addEnvironment({
            value: Buffer.from(frame.value) as Hash32,
            tail: Buffer.from(value.environment) as Hash32,
            length: (summary.kind === "empty" ? 0n : summary.length) + 1n,
          });
          this.record(
            {
              kind: "returnApplyValueLambda",
              argument: frame.value,
              body: value.body,
              closureEnvironment: value.environment,
              closureSummary: summary,
              tail: frame.tail,
            },
            exactState(pre, {
              mode: "compute",
              focusRoot: value.body,
              environmentRoot: environment,
              continuationRoot: frame.tail,
              auxiliary: 0n,
            }),
          );
          return;
        }
        if (value.kind === "builtin") {
          this.record(
            {
              kind: "returnApplyValueBuiltin",
              argument: frame.value,
              tag: value.tag,
              forcesRemaining: value.forcesRemaining,
              argumentsCount: value.argumentsCount,
              argumentsRoot: value.argumentsRoot,
              tail: frame.tail,
            },
            this.applyBuiltin(pre, frame.value, value, frame.tail),
          );
          return;
        }
        this.record(
          {
            kind: "returnApplyValueInvalid",
            argument: frame.value,
            function: value,
            tail: frame.tail,
          },
          errorSuccessor(pre, MidgardCekErrorCodes.InvalidApplication),
        );
        return;
      }
      case "force":
        if (value.kind === "delay") {
          this.record(
            {
              kind: "returnForceDelay",
              body: value.body,
              closureEnvironment: value.environment,
              tail: frame.tail,
            },
            exactState(pre, {
              mode: "compute",
              focusRoot: value.body,
              environmentRoot: value.environment,
              continuationRoot: frame.tail,
              auxiliary: 0n,
            }),
          );
          return;
        }
        if (value.kind === "builtin" && value.forcesRemaining > 0n) {
          const nextValue = this.addValue({
            ...value,
            forcesRemaining: value.forcesRemaining - 1n,
          });
          this.record(
            {
              kind: "returnForceBuiltin",
              tag: value.tag,
              forcesRemaining: value.forcesRemaining,
              argumentsCount: value.argumentsCount,
              argumentsRoot: value.argumentsRoot,
              tail: frame.tail,
            },
            exactState(pre, {
              mode: "return",
              focusRoot: nextValue,
              environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
              continuationRoot: frame.tail,
              auxiliary: 0n,
            }),
          );
          return;
        }
        this.record(
          {
            kind: "returnForceInvalid",
            value,
            tail: frame.tail,
          },
          errorSuccessor(pre, MidgardCekErrorCodes.InvalidForce),
        );
        return;
      case "constr": {
        if (frame.remainingTermsCount > 0n) {
          const next = this.sequence(
            frame.remainingTermsRoot,
            frame.remainingTermsCount,
          );
          const nextValuesCount = frame.valuesCount + 1n;
          const nextValuesRoot = this.addSequence({
            head: Buffer.from(pre.focusRoot) as Hash32,
            tail: Buffer.from(frame.valuesRoot) as Hash32,
            length: nextValuesCount,
          });
          const continuation = this.addContinuation({
            ...frame,
            remainingTermsCount: frame.remainingTermsCount - 1n,
            remainingTermsRoot: next.tail,
            valuesCount: nextValuesCount,
            valuesRoot: nextValuesRoot,
          });
          this.record(
            {
              kind: "returnConstrNext",
              tag: frame.tag,
              remainingTermsCount: frame.remainingTermsCount,
              nextTerm: next.head,
              remainingTermsTail: next.tail,
              valuesCount: frame.valuesCount,
              valuesRoot: frame.valuesRoot,
              capturedEnvironment: frame.environment,
              tail: frame.tail,
            },
            exactState(pre, {
              mode: "compute",
              focusRoot: next.head,
              environmentRoot: frame.environment,
              continuationRoot: continuation,
              auxiliary: 0n,
            }),
          );
          return;
        }
        const nextValuesCount = frame.valuesCount + 1n;
        const nextValuesRoot = this.addSequence({
          head: Buffer.from(pre.focusRoot) as Hash32,
          tail: Buffer.from(frame.valuesRoot) as Hash32,
          length: nextValuesCount,
        });
        const constr = this.addValue({
          kind: "constr",
          tag: frame.tag,
          valuesCount: nextValuesCount,
          valuesRoot: nextValuesRoot,
        });
        this.record(
          {
            kind: "returnConstrDone",
            tag: frame.tag,
            valuesCount: frame.valuesCount,
            valuesRoot: frame.valuesRoot,
            capturedEnvironment: frame.environment,
            tail: frame.tail,
          },
          exactState(pre, {
            mode: "return",
            focusRoot: constr,
            environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
            continuationRoot: frame.tail,
            auxiliary: 0n,
          }),
        );
        return;
      }
      case "case": {
        if (value.kind !== "constr") {
          this.record(
            {
              kind: "returnCaseInvalid",
              value,
              branchesCount: frame.branchesCount,
              branchesRoot: frame.branchesRoot,
              capturedEnvironment: frame.environment,
              tail: frame.tail,
            },
            errorSuccessor(pre, MidgardCekErrorCodes.InvalidCaseScrutinee),
          );
          return;
        }
        if (value.tag >= frame.branchesCount) {
          this.record(
            {
              kind: "returnCaseConstr",
              tag: value.tag,
              valuesCount: value.valuesCount,
              valuesRoot: value.valuesRoot,
              branchesCount: frame.branchesCount,
              branchesRoot: frame.branchesRoot,
              capturedEnvironment: frame.environment,
              tail: frame.tail,
            },
            errorSuccessor(pre, MidgardCekErrorCodes.CaseBranchMissing),
          );
          return;
        }
        const work = this.addContinuation({
          kind: "caseSelect",
          environment: frame.environment,
          tail: frame.tail,
          valuesCount: value.valuesCount,
        });
        this.record(
          {
            kind: "returnCaseConstr",
            tag: value.tag,
            valuesCount: value.valuesCount,
            valuesRoot: value.valuesRoot,
            branchesCount: frame.branchesCount,
            branchesRoot: frame.branchesRoot,
            capturedEnvironment: frame.environment,
            tail: frame.tail,
          },
          exactState(pre, {
            mode: "caseSelect",
            focusRoot: frame.branchesRoot,
            environmentRoot: value.valuesRoot,
            continuationRoot: work,
            auxiliary: value.tag,
          }),
        );
        return;
      }
      case "caseSelect":
      case "caseApply":
        throw new Error("case work continuation cannot be returned directly");
    }
  }

  private selectCase(): void {
    const pre = this.state;
    const frame = this.continuation(pre.continuationRoot);
    if (frame?.kind !== "caseSelect") {
      throw new Error("CEK case-select state has the wrong continuation");
    }
    // The branch walk starts at the head of all the branches, so the node
    // only has to be longer than the steps still to take.
    const branch = this.sequences.get(rootHex(pre.focusRoot));
    if (branch === undefined || branch.length <= pre.auxiliary) {
      throw new Error(`missing CEK case branch ${rootHex(pre.focusRoot)}`);
    }
    const witness: MidgardCekCoreStepWitness = {
      kind: "selectCaseBranch",
      branch: branch.head,
      remainingBranchesRoot: branch.tail,
      length: branch.length,
      capturedEnvironment: frame.environment,
      tail: frame.tail,
      valuesCount: frame.valuesCount,
    };
    if (pre.auxiliary > 0n) {
      this.record(
        witness,
        exactState(pre, {
          mode: "caseSelect",
          focusRoot: branch.tail,
          environmentRoot: pre.environmentRoot,
          continuationRoot: pre.continuationRoot,
          auxiliary: pre.auxiliary - 1n,
        }),
      );
      return;
    }
    if (frame.valuesCount === 0n) {
      this.record(
        witness,
        exactState(pre, {
          mode: "compute",
          focusRoot: branch.head,
          environmentRoot: frame.environment,
          continuationRoot: frame.tail,
          auxiliary: 0n,
        }),
      );
      return;
    }
    const continuation = this.addContinuation({
      kind: "caseApply",
      environment: frame.environment,
      builtContinuation: frame.tail,
    });
    this.record(
      witness,
      exactState(pre, {
        mode: "caseApply",
        focusRoot: pre.environmentRoot,
        environmentRoot: branch.head,
        continuationRoot: continuation,
        auxiliary: frame.valuesCount,
      }),
    );
  }

  private applyCase(): void {
    const pre = this.state;
    const frame = this.continuation(pre.continuationRoot);
    if (frame?.kind !== "caseApply") {
      throw new Error("CEK case-apply state has the wrong continuation");
    }
    const value = this.sequence(pre.focusRoot, pre.auxiliary);
    const nextContinuation = this.addContinuation({
      kind: "applyValue",
      value: value.head,
      tail: frame.builtContinuation,
    });
    const witness: MidgardCekCoreStepWitness = {
      kind: "applyCaseValue",
      value: value.head,
      remainingValuesRoot: value.tail,
      length: value.length,
      capturedEnvironment: frame.environment,
      builtContinuation: frame.builtContinuation,
    };
    if (value.length === 1n) {
      this.record(
        witness,
        exactState(pre, {
          mode: "compute",
          focusRoot: pre.environmentRoot,
          environmentRoot: frame.environment,
          continuationRoot: nextContinuation,
          auxiliary: 0n,
        }),
      );
      return;
    }
    const continuation = this.addContinuation({
      kind: "caseApply",
      environment: frame.environment,
      builtContinuation: nextContinuation,
    });
    this.record(
      witness,
      exactState(pre, {
        mode: "caseApply",
        focusRoot: value.tail,
        environmentRoot: pre.environmentRoot,
        continuationRoot: continuation,
        auxiliary: value.length - 1n,
      }),
    );
  }
}
