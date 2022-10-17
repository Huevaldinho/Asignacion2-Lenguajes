def bonitaPython(a):
    if (a.split(" "))[0] == 'constante':
        return (a.split('"'))[1]
    if (a.split(" "))[0] == 'negacion':
        return '~'+bonitaPython([a[1:]])
    if (a.split("("))[0] == 'conjuncion':
        return bonitaPython([a[1:]])+' && '+bonitaPython([a[1:]])
    if (a.split("("))[0] == 'disyuncion':
        return bonitaPython([a[1:]])+' || '+bonitaPython([a[1:]])
    if (a.split("("))[0] == 'implicacion':
        return bonitaPython([a[1:]])+' => '+bonitaPython([a[1:]])
    if (a.split("("))[0] == 'equivalencia':
        return bonitaPython([a[1:]])+' <=> '+bonitaPython([a[1:]])

print(bonitaPython('constante "p"'))