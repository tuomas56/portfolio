import xml.etree.ElementTree as ET
import sys

tree = ET.parse(sys.argv[1])
root = tree.getroot()
pl = root.find('ParametersList')
params = {param.find('Id').text: eval(param.find('Expression').text) for param in pl} if pl else {}
reactions = []
for reaction in root.find('ReactionsList'):
    rate_e = reaction.find('Rate').text
    try:
        rate = float(rate_e)
    except:
        rate = params[rate_e]
    lhs = [(int(float(reactant.attrib['stoichiometry'])), reactant.attrib['id']) for reactant in reaction.find('Reactants')]
    prods = reaction.find('Products')
    prods = [] if prods is None else prods
    rhs = [(int(float(reactant.attrib['stoichiometry'])), reactant.attrib['id']) for reactant in prods]
    reactions.append((lhs, rhs, rate))
initials = {species.find('Id').text: float(species.find('InitialPopulation').text) for species in root.find('SpeciesList')}
mult = int(input("Enter multiplier: "))
print("species:\n   ", ", ".join(["%s: %s" % (a, int(b * mult)) for (a, b) in initials.items()]))
print("reactions:")
for (lhs, rhs, rate) in reactions:
    print("   ", " + ".join([s if c == 1 else "%s * %s" % (c, s) for (c, s) in lhs]), "=>", " + ".join([s if c == 1 else "%s * %s" % (c, s) for (c, s) in rhs]) if len(rhs) else "{}", "(%0.4f);" % rate)
