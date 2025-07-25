# Deep merge function for attrsets with custom merge behavior
{ lib }:

rec {
  # Deep merge two attrsets with the following rules:
  # - Attrsets are recursively merged
  # - Lists are concatenated
  # - Strings from the second set override the first (or can be concatenated)
  # - Other types from the second set override the first
  deepMerge = a: b:
    let
      # Helper to check if a value is an attrset (but not a derivation)
      isAttrs = x: builtins.isAttrs x && !(x ? type && x.type == "derivation");
      
      # Process a single key
      mergeKey = key:
        let
          aHasKey = a ? ${key};
          bHasKey = b ? ${key};
        in
        if !aHasKey && bHasKey then b.${key}
        else if aHasKey && !bHasKey then a.${key}
        else if aHasKey && bHasKey then
          let
            aVal = a.${key};
            bVal = b.${key};
          in
          if isAttrs aVal && isAttrs bVal then
            deepMerge aVal bVal
          else if builtins.isList aVal && builtins.isList bVal then
            aVal ++ bVal
          else if builtins.isString aVal && builtins.isString bVal then
            # For strings, you can choose to override or concatenate
            # Override behavior (second value wins):
            bVal
            # Concatenation behavior (uncomment if preferred):
            # aVal + bVal
          else
            # For all other types, second value wins
            bVal
        else null;  # neither has the key (shouldn't happen with allKeys)
          
      # Get all unique keys from both attrsets
      allKeys = lib.unique ((lib.attrNames a) ++ (lib.attrNames b));
    in
    lib.genAttrs allKeys mergeKey;

  # Alternative version that concatenates strings with a separator
  deepMergeWithSep = sep: a: b:
    let
      isAttrs = x: builtins.isAttrs x && !(x ? type && x.type == "derivation");
      
      mergeKey = key:
        let
          aHasKey = a ? ${key};
          bHasKey = b ? ${key};
        in
        if !aHasKey && bHasKey then b.${key}
        else if aHasKey && !bHasKey then a.${key}
        else if aHasKey && bHasKey then
          let
            aVal = a.${key};
            bVal = b.${key};
          in
          if isAttrs aVal && isAttrs bVal then
            deepMergeWithSep sep aVal bVal
          else if builtins.isList aVal && builtins.isList bVal then
            aVal ++ bVal
          else if builtins.isString aVal && builtins.isString bVal then
            aVal + sep + bVal
          else
            bVal
        else null;  # neither has the key (shouldn't happen with allKeys)
          
      allKeys = lib.unique ((lib.attrNames a) ++ (lib.attrNames b));
    in
    lib.genAttrs allKeys mergeKey;

  # Merge multiple attrsets
  deepMergeMany = lib.foldl' deepMerge {};
  
  # Merge multiple attrsets with string separator
  deepMergeManyWithSep = sep: lib.foldl' (deepMergeWithSep sep) {};
}