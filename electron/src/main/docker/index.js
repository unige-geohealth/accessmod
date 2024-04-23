import meta from "../../../resources/docker/meta.json" assert { type: "json" };
import image from "../../../resources/docker/accessmod-docker.tar.gz?asset&asarUnpack";
meta.image_path = image;

export { meta };
