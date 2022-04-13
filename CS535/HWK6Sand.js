function a() {
  let n = [-1, 0, 1, 0, -1, -1, 0, 0, 1];
  let i = 0;
  let zipP;
  let oneP;
  while (i < n.length) {
    if (n[i] < 0) {
      if (zipP != null) {
        let hold = n[i];
        n[i] = zipP;
        zipP = hold;
      } else if (oneP != null) {
        let hold = n[i];
        n[i] = zipP;
        zipP = hold;
      }
    }
    if (n[i] == 0) {
      zipP = n[i];
    } else {
      oneP = n[i];
    }
    i++;
  }
  console.log(n);
}
a();
