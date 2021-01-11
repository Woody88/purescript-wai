var nextKey = 0n;

exports._newKey = () => {
  nextKey = nextKey + 1n;
  return nextKey;
};

exports._eqKey = x => y => {
  return x === (y);
};

exports._compareKey = x => y => {
  if (x > y)
    return 1 
  
  if (x === y)
    return 0 
  
  return 2;
};