# Documentation — Markowitz Shiny

## Objectif de l’application
Cette application aide à construire et comparer des portefeuilles “Markowitz” (moyenne–variance) à partir de données historiques :
- Téléchargement des prix (Yahoo)
- Conversion éventuelle des prix en **devise base (CHF)** via **FX Yahoo**
- Calcul des rendements, estimation de μ (rendements attendus) et Σ (covariance)
- Calcul de la **frontière efficiente**
- Identification de portefeuilles “clés” : **MVP** (variance minimale) et **Tangent** (max Sharpe)
- Affichage des allocations (poids, montants) et d’une conversion en actions entières (optionnelle)

> ⚠️ Important : ce n’est pas un conseil financier. Les résultats dépendent fortement de la période et des hypothèses.

---

## Flux de calcul (ce que fait le bouton “Calculer”)
1. **Tickers** : parsing + normalisation (alias CH → `.SW` si activé)
2. **Prix Yahoo** : téléchargement des prix ajustés (Adj Close)
3. **FX (si nécessaire)** : conversion des prix en CHF sur toute la période
4. **Rendements** : calcul des log-rendements journaliers
5. **Estimation** : annualisation de μ et Σ (≈ 252 jours/an)
6. **Robustesse (shrinkage Σ)** : optionnel, améliore la stabilité de Σ
7. **Frontière efficiente** : résout une série de problèmes d’optimisation (QP)
8. **KPIs + Tables** : affiche frontière, portefeuilles, allocations et FX

---

## Paramètres (sidebar)

### Tickers (Yahoo)
Liste de tickers séparés par espaces / virgules / retours ligne.  
Ex : `AAPL MSFT AMZN NOVN UBSG`

**Effet :**
- Plus tu mets de titres, plus tu diversifies… mais plus μ/Σ deviennent instables si la période est courte.

### Aide tickers CH (alias .SW)
Active une table d’alias (ex. `UBS` → `UBSG.SW`).

### Période
Détermine la fenêtre historique utilisée.

**Effet :**
- Période courte : plus “réactive” mais très bruitée.
- Période longue : plus stable mais peut diluer les régimes récents.

### Capital total (CHF)
Devise base **fixe = CHF** : les montants affichés sont en CHF.

### Conversion en actions
Option pour convertir des montants théoriques en **actions entières** :
- **Floor (simple)** : arrondi à l’inférieur (souvent laisse plus de cash)
- **Floor + optimisation cash** : tente d’investir le cash restant en achetant des actions “sous-pondérées” vs cible

> Remarque : si tu veux uniquement l’analyse, tu peux ignorer cette partie (mais elle est utile pour préparer un ordre manuel).

### Taux sans risque annuel (rf)
Utilisé pour le **Sharpe** :
\[
Sharpe = \frac{E[R] - r_f}{\sigma}
\]

**Effet :**
- rf ↑ ⇒ Sharpe ↓ (toutes choses égales) ⇒ peut changer le “tangent”.

### Robustesse (shrinkage Σ)
But : réduire le bruit d’estimation de la covariance Σ.

- **Aucun** : Σ brute (souvent instable)
- **Diagonal** : réduit les corrélations (ne garde que les variances)
- **Corrélation constante** : remplace les corrélations par une moyenne (souvent bon compromis)

### Intensité shrinkage (λ)
Mélange Σ brute et Σ cible :
- λ = 0 : Σ brute
- λ = 1 : Σ cible

**Effet :**
- λ ↑ : allocations souvent plus stables, moins “extrêmes”, moins sensibles au bruit.

### Poids max par titre (wmax)
Contraint chaque poids :
\[
0 \le w_i \le w_{max}, \quad \sum_i w_i = 1
\]

**Interprétation :**
- wmax = 0.50 ⇒ aucun titre ne peut dépasser 50%.
- Si wmax < 1/N (N = nb de titres), le problème devient **impossible** (on ne peut pas sommer à 1).

### Nb points frontière (ngrid)
Nombre de portefeuilles calculés le long de la frontière (plus = plus précis, mais plus lent).

---

## Sorties (ce que tu vois)

### Graphique “Frontière efficiente”
- **MVP (variance min)** : volatilité minimale.
- **Tangent (max Sharpe)** : meilleur compromis rendement/risque (par Sharpe).
- **Sélection** : portefeuille choisi via le slider.

La frontière a souvent 2 branches :
- **inefficiente** (rendement faible pour un risque donné)
- **efficiente** (rendement maximal pour un risque donné)

### KPIs (annualisés)
- **Rendement** : \(E[R]\) annualisé
- **Volatilité** : \(\sigma\) annualisée
- **Sharpe** : plus haut = meilleur “rendement excédentaire par unité de risque”

**Lecture rapide :**
- Sharpe élevé = “mieux rémunéré” pour le risque pris (selon les hypothèses).
- Sharpe faible = soit rendement faible, soit volatilité élevée, soit rf élevé.

### Tables allocations
- **Poids** : répartition cible
- **Montant** : poids × capital (en CHF)

### Table “Devises & FX”
Explique comment chaque ticker est converti en CHF :
- Devise (USD/CHF/etc.)
- FX_vers_CHF (taux utilisé)
- Dernier_prix_CHF (prix converti)

---

## Exemple fictif (pour comprendre les tables)
Capital = 10’000 CHF  
Poids cible :
- AAPL : 60% ⇒ 6’000 CHF
- NOVN.SW : 40% ⇒ 4’000 CHF

Si AAPL vaut ~200 CHF et NOVN vaut ~100 CHF :
- AAPL : floor(6000/200)=30 actions ⇒ 6’000 CHF
- NOVN : floor(4000/100)=40 actions ⇒ 4’000 CHF
Cash restant ≈ 0 CHF

Avec “optimisation cash”, si du cash reste, l’app essaie d’acheter 1 action là où le portefeuille est le plus sous-pondéré vs cible.

---

## Limites et bonnes pratiques
- μ/Σ estimés sur l’historique ⇒ très sensibles au bruit.
- Shrinkage aide beaucoup sur Σ, mais μ reste fragile.
- Pour du long-terme : privilégier une période suffisante + diversification + contraintes (wmax).
- Ne pas oublier : coûts de transaction, impôts, liquidité, etc. (non modélisés).
