export const validateTestDatabasePrefix = (prefix) => {
  if (
    !/^midgard_(?:test|tools_test|contrib)(?:_[a-z0-9]+)*$/u.test(prefix) ||
    prefix.length > 54
  ) {
    throw new Error(
      "Disposable database prefix must be midgard_test, midgard_tools_test or midgard_contrib with lowercase identity suffixes and at most 54 characters",
    );
  }
  return prefix;
};

export const disposableDatabaseName = (prefix, shard) => {
  validateTestDatabasePrefix(prefix);
  if (!/^(?:0|[1-9][0-9]{0,5})$/u.test(String(shard)))
    throw new Error(
      "Test database shard must be a bounded nonnegative integer",
    );
  return `${prefix}_w${shard}`;
};
