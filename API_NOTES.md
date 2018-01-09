
Exchange API

/api/settings/sync/progress
/api/txs/histories?limit={limit}&accountId={_account}
/api/wallets
/api/addresses/{address}
/api/txs/payments/{_account}/{to}/

Ada.cs:            string reqUrl = "/api/settings/sync/progress";
Ada.cs:            string reqUrl = $"/api/txs/histories?limit={limit}&accountId={_account}";
Ada.cs:            string reqUrl = "/api/settings/sync/progress";
Ada.cs:            string reqUrl = "/api/wallets";
Ada.cs:            string reqUrl = $"/api/addresses/{address}";
Ada.cs:            string reqUrl = $"/api/txs/payments/{_account}/{to}/{Convert.ToInt64(Math.Floor(amount * currency.BaseUnits.Value))}";
Ada.cs:            string reqUrl = "/api/addresses";
