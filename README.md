# solvate_stoichiometry_calc_app

This RShiny web application estimates the hydrate/solvate stoichiometry of crystalline solids utilizing either Thermogravimetric Analysis (TGA) or Dynamic Vapour Sorption (DVS) data. It's equipped with a built-in database containing 20 common industrial solvents and the option to specify an additional solvent by providing its molecular weight (Mw).

With two distinct modes, users can seamlessly navigate between functionalities. The first mode, accessible via the "Stoichiometry" radio button, enables users to compute the stoichiometry corresponding to the TGA/DVS weight loss. In the second mode, activated through the "Weight Chng %" radio button, the application calculates the weight loss corresponding to a list of prevalent stoichiometries observed in the crystal structure of small organic molecules. Additionally, users have the option to highlight values falling within a specified search range, enhancing result interpretation and analysis.

The application solvent database includes: water, acetone, acetonitrile, dmso, ethanol, ethyl acetate, ethylene glycol, glycerol, heptane, hexane, mek, methanol, mibk, mtbe, octane, propanol, thf, toluene, xylene, 2-MeTHF.

Mode **Stoichiometry**:  

<img src="images/solvate_stoichiometry_app_1.png>

Mode **Weight Chng %**:  

<img src="images/solvate_stoichiometry_app_2.png>
