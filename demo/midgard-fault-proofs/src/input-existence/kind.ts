/** An input-existence family: the input a forced transaction's InputNotFound
 * rejection names, a spent input (source kind 0, the spend-input field) or a
 * reference input (source kind 1, the reference-input field). */
export type InputExistenceKind<
  Category extends InputExistenceCategory = InputExistenceCategory,
  ViolationId extends string = string,
> = Readonly<{
  /** The family's fault-proof category, which prefixes its proofs' errors. */
  category: Category;
  /** The family's hyphenated name, which prefixes its workflow's errors. */
  name: string;
  violationId: ViolationId;
  /** The schema version of the family's forced artifact. */
  forcedArtifact: string;
  /** The Plutus data schema of the family's forced source. */
  forcedSourceSchema: unknown;
  sourceKind: bigint;
  /** The transaction field that lists inputs of this kind. */
  fieldIndex: 0 | 1;
}>;

export type InputExistenceCategory = "noReferenceInput" | "nonExistentInput";
